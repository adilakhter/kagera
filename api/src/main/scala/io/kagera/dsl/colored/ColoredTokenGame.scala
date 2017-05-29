package io.kagera.dsl.colored

import io.kagera.api.{ Marking, MultiSet, ReferenceTokenGame }

class ColoredTokenGame extends ReferenceTokenGame[Place, Transition] {
  def consumableTokens(petriNet: ColoredPetriNet)(marking: Marking[Place], p: Place[_], t: Transition[_, _, _]): MultiSet[_] = {
    val edge = petriNet.getEdge(p, t).get
    marking.get(p) match {
      case None         ⇒ MultiSet.empty
      case Some(tokens) ⇒ tokens.filter { case (e, count) ⇒ edge.filter(e) }
    }
  }
}
