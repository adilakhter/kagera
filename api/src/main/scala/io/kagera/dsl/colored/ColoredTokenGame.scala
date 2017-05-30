package io.kagera.dsl.colored

import io.kagera.api.{ Marking, MultiSet, ReferenceTokenGame }

class ColoredTokenGame extends ReferenceTokenGame[Place, Transition] {
  def consumableTokens(petriNet: ColoredPetriNet)(marking: Marking[Place], p: Place[_], t: Transition[_, _]): MultiSet[_] = marking.getOrElse(p, MultiSet.empty)
}
