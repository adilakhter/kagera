package io.kagera.api

class PetriNetAnalysis[P, T](pn: PetriNet[P, T]) {

  val inMarking = pn.transitions.map(t ⇒ t -> pn.inMarking(t)).toMap
  val outMarking = pn.transitions.map(t ⇒ t -> pn.outMarking(t)).toMap

  val coldTransitions = pn.transitions.filter(t ⇒ pn.incomingPlaces(t).isEmpty)

  def enabledTransitions(m0: MultiSet[P]): Set[T] = {

    val outAdjancent = m0.keys.map(pn.outgoingTransitions).reduceOption(_ ++ _).getOrElse(Set.empty).
      filter(t ⇒ m0.isSubSet(inMarking(t)))

    coldTransitions ++ outAdjancent
  }

  def enabledPermutations(m: MultiSet[P]): Set[Set[T]] = {
    enabledTransitions(m)
      .map(t ⇒ enabledPermutations(m.multisetDifference(inMarking(t)))
        .map(_ + t) + Set(t)).reduceOption(_ ++ _).getOrElse(Set.empty)
  }

  def fire(m0: MultiSet[P], t: T): MultiSet[P] =
    m0.multisetDifference(inMarking(t))
      .multisetSum(outMarking(t))

  def fireAll(m0: MultiSet[P], transitions: Iterable[T]): MultiSet[P] = {
    transitions.foldLeft(m0) {
      case (m, t) ⇒ fire(m, t)
    }
  }

  def isCoverable(marking: MultiSet[P], target: MultiSet[P]): Boolean = {

    if (marking.isSubSet(target))
      true
    else
      enabledPermutations(marking).view
        .map(t ⇒ isCoverable(fireAll(marking, t), target))
        .exists(_ == true)
  }
}
