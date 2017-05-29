package io.kagera.api

trait ReferenceTokenGame[P[_], T[_, _, _]] extends TokenGame[P[_], T[_, _, _], Marking[P]] {

  override def enabledParameters(petriNet: PetriNet[P[_], T[_, _, _]])(m: Marking[P]): Map[T[_, _, _], Iterable[Marking[P]]] =
    enabledTransitions(petriNet)(m).view.map(t ⇒ t -> consumableMarkings(petriNet)(m, t)).toMap

  def consumableMarkings(petriNet: PetriNet[P[_], T[_, _, _]])(marking: Marking[P], t: T[_, _, _]): Iterable[Marking[P]] = {
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

  def consumableTokens(petriNet: PetriNet[P[_], T[_, _, _]])(marking: Marking[P], p: P[_], t: T[_, _, _]): MultiSet[_]

  // TODO optimize, no need to process all transitions
  override def enabledTransitions(petriNet: PetriNet[P[_], T[_, _, _]])(marking: Marking[P]): Set[T[_, _, _]] =
    petriNet.transitions.filter(t ⇒ consumableMarkings(petriNet)(marking, t).nonEmpty)
}
