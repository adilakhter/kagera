package io.kagera.akka.actor

import akka.actor._
import akka.cluster.sharding.ShardRegion.Passivate
import akka.event.Logging
import akka.event.Logging.LogLevel
import akka.pattern.pipe
import fs2.{ Strategy, Task }
import io.kagera.akka.actor.PetriNetInstance.Settings
import io.kagera.akka.actor.PetriNetInstanceProtocol._
import io.kagera.api._
import io.kagera.execution.EventSourcing._
import io.kagera.execution.ExceptionStrategy.RetryWithDelay
import io.kagera.execution._
import io.kagera.persistence.ObjectSerializer

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.language.existentials

object PetriNetInstance {

  val persistencePrefix = "process-"

  case class Settings(
    evaluationStrategy: Strategy,
    idleTTL: Option[FiniteDuration],
    serializer: ObjectSerializer)

  private case class IdleStop(seq: Long)

  def processId2PersistenceId(processId: String): String = s"$persistencePrefix$processId"
  def persistenceId2ProcessId(persistenceId: String): Option[String] = {
    if (persistenceId startsWith persistencePrefix) Option(persistenceId.split(persistencePrefix)(1))
    else None
  }
}

/**
 * This actor is responsible for maintaining the state of a single petri net instance.
 */
class PetriNetInstance[P[_], T[_, _], S](
    topology: PetriNet[P[_], T[_, _]],
    settings: Settings,
    runtime: PetriNetRuntime[P, T, S, Any],
    override implicit val placeIdentifier: Identifiable[P[_]],
    override implicit val transitionIdentifier: Identifiable[T[_, _]]) extends PetriNetInstanceRecovery[P, T, S](topology, settings.serializer, runtime.eventSourceFn) {

  import PetriNetInstance._

  val processId = context.self.path.name

  val log = Logging.getLogger(this)

  override def persistenceId: String = processId2PersistenceId(processId)

  import context.dispatcher

  val executor = runtime.jobExecutor.apply(topology)(settings.evaluationStrategy)

  override def receiveCommand = uninitialized

  def logWithMDC(level: LogLevel, msg: String, mdc: Map[String, Any]) = {
    try {
      log.setMDC(mdc.asJava)
      log.log(level, msg)
    } finally {
      log.clearMDC()
    }
  }

  def uninitialized: Receive = {
    case msg @ Initialize(markingData, state) ⇒

      val initialMarking = Marking.unmarshal[P](markingData, topology.places.getById)
      val uninitialized = Instance.uninitialized[P, T, S](topology)
      val event = InitializedEvent(initialMarking, state)

      persistEvent(uninitialized, event) {
        eventSource.apply(uninitialized)
          .andThen(step)
          .andThen { updatedInstance ⇒
            context become running(updatedInstance._1, Map.empty)
            sender() ! Initialized(Marking.marshal(initialMarking), state)
          }
      }
    case msg: Command ⇒
      sender() ! Uninitialized(processId)
      context.parent ! Passivate(SupervisorStrategy.Stop)

    case SupervisorStrategy.Stop ⇒
      context.stop(context.self)
  }

  def running(instance: Instance[P, T, S],
    scheduledRetries: Map[Long, Cancellable]): Receive = {

    case SupervisorStrategy.Stop ⇒
      scheduledRetries.values.foreach(_.cancel())
      context.stop(context.self)

    case IdleStop(n) if n == instance.sequenceNr && instance.activeJobs.isEmpty ⇒
      val mdc = Map("processId" -> processId)
      logWithMDC(Logging.DebugLevel, s"Instance was idle for ${settings.idleTTL}, stopping the actor", mdc)
      context.parent ! Passivate(SupervisorStrategy.Stop)

    case GetState ⇒
      sender() ! fromExecutionInstance(instance)

    case event @ TransitionFiredEvent(jobId, transition, timeStarted, timeCompleted, consumed, produced, output) ⇒

      val transitionId = transitionIdentifier(transition.asInstanceOf[T[_, _]]).value

      val mdc = Map(
        "kageraEvent" -> "TransitionFired",
        "processId" -> processId,
        "jobId" -> jobId,
        "transitionId" -> transitionId,
        "timeStarted" -> timeStarted,
        "timeCompleted" -> timeCompleted,
        "duration" -> (timeCompleted - timeStarted)
      )

      logWithMDC(Logging.DebugLevel, s"Transition '$transition' successfully fired", mdc)

      persistEvent(instance, event)(
        eventSource.apply(instance)
          .andThen(step)
          .andThen {
            case (updatedInstance, newJobs) ⇒
              sender() ! TransitionFired(jobId, transitionId, Marking.marshal[P](event.consumed.asInstanceOf[Marking[P]]), Marking.marshal[P](event.produced.asInstanceOf[Marking[P]]), fromExecutionInstance(updatedInstance), newJobs.map(_.id))
              context become running(updatedInstance, scheduledRetries - jobId)
              updatedInstance
          }
      )

    case event @ TransitionFailedEvent(jobId, transition, timeStarted, timeFailed, consume, input, reason, strategy) ⇒

      val transitionId = transitionIdentifier(transition.asInstanceOf[T[_, _]]).value

      val mdc = Map(
        "kageraEvent" -> "TransitionFailed",
        "processId" -> processId,
        "timeStarted" -> timeStarted,
        "timeFailed" -> timeFailed,
        "duration" -> (timeFailed - timeStarted),
        "transitionId" -> transitionId,
        "transitionFailureReason" -> reason
      )

      logWithMDC(Logging.WarningLevel, s"Transition '$transition' failed with: $reason", mdc)

      strategy match {
        case RetryWithDelay(delay) ⇒
          val mdc = Map(
            "kageraEvent" -> "TransitionRetry",
            "processId" -> processId,
            "transitionId" -> transitionId)

          logWithMDC(Logging.WarningLevel, s"Scheduling a retry of transition '$transition' in ${Duration(delay, MILLISECONDS).toString()}", mdc)

          val originalSender = sender()
          persistEvent(instance, event)(
            eventSource.apply(instance)
              .andThen { updatedInstance ⇒
                val retry = system.scheduler.scheduleOnce(delay milliseconds) { executeJob(updatedInstance.jobs(jobId), originalSender) }
                sender() ! TransitionFailed(jobId, transitionId, Marking.marshal[P](consume.asInstanceOf[Marking[P]]), input, reason, strategy)
                context become running(updatedInstance, scheduledRetries + (jobId -> retry))
              }
          )

        case _ ⇒
          persistEvent(instance, event)(
            eventSource.apply(instance)
              .andThen { updatedInstance ⇒
                sender() ! TransitionFailed(jobId, transitionId, Marking.marshal[P](consume.asInstanceOf[Marking[P]]), input, reason, strategy)
                context become running(updatedInstance, scheduledRetries - jobId)
              })
      }

    case msg @ FireTransition(transitionId, input, correlationId) ⇒

      val transition = topology.transitions.getById(transitionId)

      runtime.jobPicker.createJob[S, Any, Any](transition.asInstanceOf[T[Any, Any]], input).run(instance).value match {
        case (updatedInstance, Right(job)) ⇒
          executeJob(job, sender())
          context become running(updatedInstance, scheduledRetries)
        case (_, Left(reason)) ⇒
          val mdc = Map(
            "kageraEvent" -> "FireTransitionRejected",
            "processId" -> processId,
            "transitionId" -> transitionId,
            "rejectReason" -> reason)

          logWithMDC(Logging.WarningLevel, s"Not Firing Transition '$transition' because: $reason", mdc)
          sender() ! TransitionNotEnabled(transitionId, reason)
      }
    case msg: Initialize ⇒
      sender() ! AlreadyInitialized
  }

  // TODO remove side effecting here
  def step(instance: Instance[P, T, S]): (Instance[P, T, S], Set[Job[P, T, S, _]]) = {

    runtime.jobPicker.enabledJobs.run(instance).value match {
      case (updatedInstance, jobs) ⇒

        if (jobs.isEmpty && updatedInstance.activeJobs.isEmpty)
          settings.idleTTL.foreach { ttl ⇒
            system.scheduler.scheduleOnce(ttl, context.self, IdleStop(updatedInstance.sequenceNr))
          }

        jobs.foreach(job ⇒ executeJob(job, sender()))
        (updatedInstance, jobs)
    }
  }

  def executeJob[E](job: Job[P, T, S, E], originalSender: ActorRef) = {

    val transitionId = transitionIdentifier(job.transition.asInstanceOf[T[_, _]]).value
    val mdc = Map(
      "kageraEvent" -> "FiringTransition",
      "processId" -> processId,
      "jobId" -> job.id,
      "transitionId" -> transitionId,
      "timeStarted" -> System.currentTimeMillis()
    )

    logWithMDC(Logging.DebugLevel, s"Firing transition ${job.transition}", mdc)
    executor(job).unsafeRunAsyncFuture().pipeTo(context.self)(originalSender)
  }

  def scheduleFailedJobsForRetry(instance: Instance[P, T, S]): Map[Long, Cancellable] = {
    instance.jobs.values.foldLeft(Map.empty[Long, Cancellable]) {
      case (map, j @ Job(_, _, _, _, _, Some(io.kagera.execution.ExceptionState(failureTime, _, _, RetryWithDelay(delay))))) ⇒
        val newDelay = failureTime + delay - System.currentTimeMillis()
        if (newDelay < 0) {
          executeJob(j, sender())
          map
        } else {
          val cancellable = system.scheduler.scheduleOnce(newDelay milliseconds) {
            executeJob(j, sender())
          }
          map + (j.id -> cancellable)
        }
      case (acc, _) ⇒ acc
    }
  }

  override def onRecoveryCompleted(instance: Instance[P, T, S]) = {
    val scheduledRetries = scheduleFailedJobsForRetry(instance)

    val updatedInstance = step(instance)._1
    context become running(updatedInstance, scheduledRetries)
  }
}
