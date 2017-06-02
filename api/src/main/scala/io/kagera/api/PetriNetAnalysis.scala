package io.kagera.api

object PetriNetAnalysis {

  implicit class PetriNetAnalysisFunctions[P, T](pn: PetriNet[P, T]) {

    def isReachable(start: MultiSet[P], target: MultiSet[P]): Boolean = PetriNetAnalysis.isReachable(pn)(start, target)

    def isCoverable(start: MultiSet[P], target: MultiSet[P]): Boolean = ???

    def enabledTransitions(m: MultiSet[P]): Iterable[T] = PetriNetAnalysis.enabledTransitions(pn)(m)
  }

  private def markingAfterT[P, T](pn: PetriNet[P, T])(m0: MultiSet[P], t: T): MultiSet[P] =
    m0.multisetDifference(pn.inMarking(t))
      .multisetSum(pn.outMarking(t))

  /**
   * Given a petri net and a marking, finds the enabled transitions in that marking.
   */
  def enabledTransitions[P, T](pn: PetriNet[P, T])(m0: MultiSet[P]): Set[T] = {

    // transitions without input may always fire
    val c = pn.transitions.filter(t ⇒ pn.incomingPlaces(t).isEmpty)

    val outT = m0.keys.map(pn.outgoingTransitions).reduceOption(_ ++ _).getOrElse(Set.empty).
      filter(t ⇒ m0.isSubSet(pn.inMarking(t)))

    c ++ outT
  }

  def isReachable[P, T](pn: PetriNet[P, T])(start: MultiSet[P], target: MultiSet[P]): Boolean = {
    if (start == target)
      true
    else
      enabledTransitions(pn)(start).view.map { t ⇒
        val nextMarking = markingAfterT(pn)(start, t)
        isReachable(pn)(nextMarking, target) }.exists(_ == true)
  }
}
