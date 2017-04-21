package io.kagera.execution

import java.io.{ PrintWriter, StringWriter }

import fs2.{ Strategy, Task }
import io.kagera.api._
import io.kagera.api.colored._
import io.kagera.execution.EventSourcing._

class JobExecutor[State, P[_], T[_, _, _]](
    topology: PetriNet[P[_], T[_, _, _]],
    taskProvider: TransitionTaskProvider[State, P, T],
    exceptionHandlerFn: T[_, _, _] ⇒ TransitionExceptionHandler)(implicit strategy: Strategy, id: Identifiable[T[_, _, _]]) {

  val cachedTransitionTasks: Map[T[_, _, _], _] =
    topology.transitions.map(t ⇒ t -> taskProvider.apply[Any, Any](topology.inMarking(t), topology.outMarking(t), t.asInstanceOf[T[Any, Any, State]])).toMap

  def transitionFunction[Input, Output](t: T[Input, Output, State]) =
    cachedTransitionTasks(t).asInstanceOf[TransitionTask[Input, Output, State]]

  def executeTransitionAsync[Input, Output](t: T[Input, Output, State]): TransitionTask[Input, Output, State] = {
    (consume, state, input) ⇒

      val handleFailure: PartialFunction[Throwable, Task[(Marking, Output)]] = {
        case e: Throwable ⇒ Task.fail(e)
      }

      if (consume.multiplicities != topology.inMarking(t)) {
        Task.fail(new IllegalArgumentException(s"Transition $t may not consume $consume"))
      }

      try {
        transitionFunction(t)(consume, state, input).handleWith { handleFailure }.async
      } catch { handleFailure }
  }

  /**
   * Executes a job returning a Task[TransitionEvent]
   */
  def apply[E](job: Job[State, E]): Task[TransitionEvent] = {

    val startTime = System.currentTimeMillis()
    val transition = topology.transitions.getById(job.transitionId).asInstanceOf[T[Any, Any, State]]

    executeTransitionAsync(transition)(job.consume, job.processState, job.input).map {
      case (produced, out) ⇒
        TransitionFiredEvent(job.id, job.transitionId, startTime, System.currentTimeMillis(), job.consume, produced, Some(out))
    }.handle {
      case e: Throwable ⇒
        val failureCount = job.failureCount + 1
        val failureStrategy = exceptionHandlerFn(transition).apply(e, failureCount)

        val sw = new StringWriter()
        e.printStackTrace(new PrintWriter(sw))
        val stackTraceString = sw.toString

        TransitionFailedEvent(job.id, job.transitionId, startTime, System.currentTimeMillis(), job.consume, Some(job.input), stackTraceString, failureStrategy)
    }
  }
}
