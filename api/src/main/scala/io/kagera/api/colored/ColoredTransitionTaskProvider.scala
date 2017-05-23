package io.kagera.api.colored

import io.kagera.api.MultiSet
import io.kagera.execution.{ TransitionTask, TransitionTaskProvider }

class ColoredTransitionTaskProvider[S] extends TransitionTaskProvider[S, Place, Transition] {
  override def apply[Input, Output](inAdjacent: MultiSet[Place[_]], outAdjacent: MultiSet[Place[_]], t: Transition[Input, Output, S]): TransitionTask[Place, Input, Output, S] =
    t.apply(inAdjacent, outAdjacent)
}
