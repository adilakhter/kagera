package io.kagera.execution

import io.kagera.api.multiset._

trait TransitionTaskProvider[State, P[_], T[_, _, _]] {

  /**
   * Given a transition returns an TransitionTask
   *
   * @tparam Input  The input type of the transition.
   * @tparam Output The output type of the transition.
   * @param t       The transition.
   * @return
   */
  def apply[Input, Output](inAdjacent: MultiSet[P[_]], outAdjacent: MultiSet[P[_]], t: T[Input, Output, State]): TransitionTask[Input, Output, State]
}