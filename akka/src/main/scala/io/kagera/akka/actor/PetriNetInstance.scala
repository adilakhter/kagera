package io.kagera.akka.actor

import akka.actor._
import akka.cluster.sharding.ShardRegion.Passivate
import akka.event.Logging
import akka.event.Logging.LogLevel
import akka.pattern.pipe
import fs2.Strategy
import io.kagera.akka.actor.PetriNetInstance.Settings
import io.kagera.akka.actor.PetriNetInstanceProtocol._
import io.kagera.api._
import io.kagera.api.colored.ExceptionStrategy.RetryWithDelay
import io.kagera.api.colored._
import io.kagera.execution.EventSourcing._
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

  def props[S](topology: ExecutablePetriNet[S], settings: Settings): Props =
    Props(new PetriNetInstance[S](topology, settings, new AsyncTransitionExecutor[S](topology)(settings.evaluationStrategy)))
}

/**
 * This actor is responsible for maintaining the state of a single petri net instance.
 */
class PetriNetInstance[S](
    topology: ExecutablePetriNet[S],
    settings: Settings,
    executor: TransitionExecutor[S, Transition]) extends PetriNetInstanceRecovery[S](topology, settings.serializer) {

  import PetriNetInstance._

  val processId = context.self.path.name

  val log = Logging.getLogger(this)

  override def persistenceId: String = processId2PersistenceId(processId)

  import context.dispatcher

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
    case msg @ Initialize(marking, state) ⇒
      val uninitialized = Instance.uninitialized[S](topology)
      val event = InitializedEvent(marking, state)
      persistEvent(uninitialized, event) {
        EventSourcing.apply(uninitialized)
          .andThen(step)
          .andThen { updatedInstance ⇒
            context become running(updatedInstance._1, Map.empty)
            sender() ! Initialized(marking, state)
          }
      }
    case msg: Command ⇒
      sender() ! Uninitialized(processId)
      context.parent ! Passivate(SupervisorStrategy.Stop)

    case SupervisorStrategy.Stop ⇒
      context.stop(context.self)
  }

  def running(instance: Instance[S], scheduledRetries: Map[Long, Cancellable]): Receive = {

    case SupervisorStrategy.Stop ⇒
      scheduledRetries.values.foreach(_.cancel())
      context.stop(context.self)

    case IdleStop(n) if n == instance.sequenceNr && instance.activeJobs.isEmpty ⇒
      val mdc = Map("processId" -> processId)
      logWithMDC(Logging.DebugLevel, s"Instance was idle for ${settings.idleTTL}, stopping the actor", mdc)
      context.parent ! Passivate(SupervisorStrategy.Stop)

    case GetState ⇒
      sender() ! fromExecutionInstance(instance)

    case event @ TransitionFiredEvent(jobId, transitionId, timeStarted, timeCompleted, consumed, produced, output) ⇒

      val transition = topology.transitions.getById(transitionId)
      val mdc = Map(
        "kageraEvent" -> "TransitionFired",
        "processId" -> processId,
        "jobId" -> jobId,
        "transitionId" -> transition.id,
        "transitionLabel" -> transition.label,
        "timeStarted" -> timeStarted,
        "timeCompleted" -> timeCompleted,
        "duration" -> (timeCompleted - timeStarted)
      )

      logWithMDC(Logging.DebugLevel, s"Transition '$transition' successfully fired", mdc)

      persistEvent(instance, event)(
        EventSourcing.apply(instance)
          .andThen(step)
          .andThen {
            case (updatedInstance, newJobs) ⇒
              sender() ! TransitionFired(jobId, transitionId, event.consumed, event.produced, fromExecutionInstance(updatedInstance), newJobs.map(_.id))
              context become running(updatedInstance, scheduledRetries - jobId)
              updatedInstance
          }

      )

    case event @ TransitionFailedEvent(jobId, transitionId, timeStarted, timeFailed, consume, input, reason, strategy) ⇒

      val transition = topology.transitions.getById(transitionId)

      val mdc = Map(
        "kageraEvent" -> "TransitionFailed",
        "processId" -> processId,
        "timeStarted" -> timeStarted,
        "timeFailed" -> timeFailed,
        "duration" -> (timeFailed - timeStarted),
        "transitionId" -> transition.id,
        "transitionLabel" -> transition.label,
        "transitionFailureReason" -> reason
      )

      logWithMDC(Logging.WarningLevel, s"Transition '$transition' failed with: $reason", mdc)

      strategy match {
        case RetryWithDelay(delay) ⇒
          val mdc = Map(
            "kageraEvent" -> "TransitionRetry",
            "processId" -> processId,
            "transitionId" -> transition.id,
            "transitionLabel" -> transition.label)

          logWithMDC(Logging.WarningLevel, s"Scheduling a retry of transition '$transition' in ${Duration(delay, MILLISECONDS).toString()}", mdc)
          val originalSender = sender()
          persistEvent(instance, event)(
            EventSourcing.apply(instance)
              .andThen { updatedInstance ⇒
                val retry = system.scheduler.scheduleOnce(delay milliseconds) { executeJob(updatedInstance.jobs(jobId), originalSender) }
                sender() ! TransitionFailed(jobId, transitionId, consume, input, reason, strategy)
                context become running(updatedInstance, scheduledRetries + (jobId -> retry))
              }
          )

        case _ ⇒
          persistEvent(instance, event)(
            EventSourcing.apply(instance)
              .andThen { updatedInstance ⇒
                sender() ! TransitionFailed(jobId, transitionId, consume, input, reason, strategy)
                context become running(updatedInstance, scheduledRetries - jobId)
              })
      }

    case msg @ FireTransition(transitionId, input, correlationId) ⇒

      val transition = topology.transitions.getById(transitionId)

      fireTransitionById[S](transitionId, input).run(instance).value match {
        case (updatedInstance, Right(job)) ⇒
          executeJob(job, sender())
          context become running(updatedInstance, scheduledRetries)
        case (_, Left(reason)) ⇒
          val mdc = Map(
            "kageraEvent" -> "FireTransitionRejected",
            "processId" -> processId,
            "transitionId" -> transition.id,
            "transitionLabel" -> transition.label,
            "rejectReason" -> reason)

          logWithMDC(Logging.WarningLevel, s"Not Firing Transition '$transition' because: $reason", mdc)
          sender() ! TransitionNotEnabled(transitionId, reason)
      }
    case msg: Initialize ⇒
      sender() ! AlreadyInitialized
  }

  // TODO remove side effecting here
  def step(instance: Instance[S]): (Instance[S], Set[Job[S, _]]) = {

    newEnabledJobs.run(instance).value match {
      case (updatedInstance, jobs) ⇒

        if (jobs.isEmpty && updatedInstance.activeJobs.isEmpty)
          settings.idleTTL.foreach { ttl ⇒

            system.scheduler.scheduleOnce(ttl, context.self, IdleStop(updatedInstance.sequenceNr))
          }

        jobs.foreach(job ⇒ executeJob(job, sender()))
        (updatedInstance, jobs)
    }
  }

  def executeJob[E](job: Job[S, E], originalSender: ActorRef) = {

    val mdc = Map(
      "kageraEvent" -> "FiringTransition",
      "processId" -> processId,
      "jobId" -> job.id,
      "transitionId" -> job.transition.id,
      "transitionLabel" -> job.transition.label,
      "timeStarted" -> System.currentTimeMillis()
    )

    logWithMDC(Logging.DebugLevel, s"Firing transition ${job.transition}", mdc)
    runJobAsync(executor)(job).unsafeRunAsyncFuture().pipeTo(context.self)(originalSender)
  }

  def scheduleFailedJobsForRetry(instance: Instance[S]): Map[Long, Cancellable] = {
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

  override def onRecoveryCompleted(instance: Instance[S]) = {
    val scheduledRetries = scheduleFailedJobsForRetry(instance)

    val updatedInstance = step(instance)._1
    context become running(updatedInstance, scheduledRetries)
  }
}
