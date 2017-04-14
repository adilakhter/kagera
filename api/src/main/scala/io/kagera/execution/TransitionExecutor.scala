package io.kagera.execution

import fs2.{ Strategy, Task }
import io.kagera.api.colored.{ Transition, _ }

class TransitionExecutor[State](topology: ColoredPetriNet, taskProvider: TransitionTaskProvider[State, Place, Transition])(implicit strategy: Strategy) {

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
}
