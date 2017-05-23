package io.kagera.api.colored

import io.kagera.api.PetriNet
import io.kagera.execution.{ TransitionTask, TransitionTaskProvider }

class ColoredTransitionTaskProvider[S] extends TransitionTaskProvider[S, Place, Transition] {
  override def apply[Input, Output](petriNet: PetriNet[Place[_], Transition[_, _, _]], t: Transition[Input, Output, S]): TransitionTask[Place, Input, Output, S] =
    t.apply(petriNet.inMarking(t), petriNet.outMarking(t))
}
