package io.kagera.api.colored

import io.kagera.api._
import io.kagera.api.multiset._

class ColoredTokenGame extends TokenGame[Place[_], Transition[_, _, _], Marking[Place]] {

  override def enabledParameters(petriNet: ColoredPetriNet)(m: Marking[Place]): Map[Transition[_, _, _], Iterable[Marking[Place]]] =
    enabledTransitions(petriNet)(m).view.map(t ⇒ t -> consumableMarkings(petriNet)(m, t)).toMap

  def consumableMarkings(petriNet: ColoredPetriNet)(marking: Marking[Place], t: Transition[_, _, _]): Iterable[Marking[Place]] = {
    // TODO this is not the most efficient, should break early when consumable tokens < edge weight
    val consumable = petriNet.inMarking(t).map {
      case (place, count) ⇒ (place, count, consumableTokens(petriNet)(marking, place, t))
    }

    // check if any
    if (consumable.exists { case (place, count, tokens) ⇒ tokens.multisetSize < count })
      Seq.empty
    else {
      val consume = consumable.map {
        case (place, count, tokens) ⇒ place -> MultiSet.from(tokens.allElements.take(count))
      }.toMarking

      // TODO lazily compute all permutations instead of only providing the first result
      Seq(consume)
    }
  }

  def consumableTokens[C](petriNet: ColoredPetriNet)(marking: Marking[Place], p: Place[C], t: Transition[_, _, _]): MultiSet[C] = {

    val edge = petriNet.getEdge(p, t).get

    marking.get(p) match {
      case None         ⇒ MultiSet.empty
      case Some(tokens) ⇒ tokens.filter { case (e, count) ⇒ edge.filter(e) }
    }
  }

  // TODO optimize, no need to process all transitions
  override def enabledTransitions(petriNet: ColoredPetriNet)(marking: Marking[Place]): Set[Transition[_, _, _]] =
    petriNet.transitions.filter(t ⇒ consumableMarkings(petriNet)(marking, t).nonEmpty)
}
