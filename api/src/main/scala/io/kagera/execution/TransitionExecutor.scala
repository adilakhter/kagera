package io.kagera.execution

import java.io.{ PrintWriter, StringWriter }

import fs2.{ Strategy, Task }
import io.kagera.api._
import io.kagera.api.colored.{ Transition, _ }
import io.kagera.execution.EventSourcing.{ TransitionEvent, TransitionFailedEvent, TransitionFiredEvent }

class TransitionExecutor[State](
    topology: ColoredPetriNet,
    taskProvider: TransitionTaskProvider[State, Place, Transition],
    exceptionHandlerFn: Transition[_, _, _] ⇒ TransitionExceptionHandler)(implicit strategy: Strategy) {

  val cachedTransitionFunctions: Map[Transition[_, _, _], _] =
    topology.transitions.map(t ⇒ t -> taskProvider.apply[Any, Any](topology.inMarking(t), topology.outMarking(t), t.asInstanceOf[Transition[Any, Any, State]])).toMap

  def transitionFunction[Input, Output](t: Transition[Input, Output, State]) =
    cachedTransitionFunctions(t).asInstanceOf[TransitionTask[Input, Output, State]]

  def apply[Input, Output](t: Transition[Input, Output, State]): TransitionTask[Input, Output, State] = {
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
  def runJobAsync[E](job: Job[State, E]): Task[TransitionEvent] = {

    val startTime = System.currentTimeMillis()
    val transition = topology.transitions.getById(job.transitionId).asInstanceOf[Transition[Any, Any, State]]

    apply(transition)(job.consume, job.processState, job.input).map {
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
