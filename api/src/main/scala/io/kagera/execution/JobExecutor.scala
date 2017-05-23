package io.kagera.execution

import java.io.{ PrintWriter, StringWriter }

import fs2.{ Strategy, Task }
import io.kagera.api._
import io.kagera.execution.EventSourcing._

/**
 * Class responsible for 'executing' a transition 'Job'
 */
class JobExecutor[State, P[_], T[_, _, _]](
    topology: PetriNet[P[_], T[_, _, _]],
    taskProvider: TransitionTaskProvider[State, P, T],
    exceptionHandlerFn: T[_, _, _] ⇒ TransitionExceptionHandler)(implicit strategy: Strategy) {

  val cachedTransitionTasks: Map[T[_, _, _], _] =
    topology.transitions.map(t ⇒ t -> taskProvider.apply[Any, Any](topology.inMarking(t), topology.outMarking(t), t.asInstanceOf[T[Any, Any, State]])).toMap

  def transitionFunction[Input, Output](t: T[Input, Output, State]) =
    cachedTransitionTasks(t).asInstanceOf[TransitionTask[P, Input, Output, State]]

  def executeTransitionAsync[Input, Output](t: T[Input, Output, State]): TransitionTask[P, Input, Output, State] = {
    (consume, state, input) ⇒

      val handleFailure: PartialFunction[Throwable, Task[(Marking[P], Output)]] = {
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
  def apply[E](job: Job[P, T, State, E]): Task[TransitionEvent[T]] = {

    val startTime = System.currentTimeMillis()
    val transition = job.transition.asInstanceOf[T[Any, Any, State]]

    executeTransitionAsync(transition)(job.consume, job.processState, job.input).map {
      case (produced, out) ⇒
        TransitionFiredEvent(job.id, transition, startTime, System.currentTimeMillis(), job.consume, produced, out)
    }.handle {
      case e: Throwable ⇒
        val failureCount = job.failureCount + 1
        val failureStrategy = exceptionHandlerFn(transition).apply(e, failureCount)

        val sw = new StringWriter()
        e.printStackTrace(new PrintWriter(sw))
        val stackTraceString = sw.toString

        TransitionFailedEvent(job.id, transition, startTime, System.currentTimeMillis(), job.consume, job.input, stackTraceString, failureStrategy)
    }
  }
}
