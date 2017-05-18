package io.kagera.api

/**
 * Interface for deciding which (transition, marking) parameters are 'enabled' or 'fireable' in a petri net.
 *
 * @tparam P Place
 * @tparam T Transition
 * @tparam M Marking     The type of Marking in the PetriNet
 */
trait TokenGame[P, T, M] {

  def enabledParameters(petriNet: PetriNet[P, T])(marking: M): Map[T, Iterable[M]] = {
    // TODO inefficient, fix
    enabledTransitions(petriNet)(marking).view.map(t ⇒ t -> consumableMarkings(petriNet)(marking, t)).toMap
  }

  def consumableMarkings(petriNet: PetriNet[P, T])(marking: M, t: T): Iterable[M]

  /**
   * Checks whether a transition is 'enabled' in a marking.
   *
   * @param marking The marking.
   * @param t The transition.
   * @return
   */
  def isEnabled(petriNet: PetriNet[P, T])(marking: M, t: T): Boolean = consumableMarkings(petriNet)(marking, t).nonEmpty

  /**
   * Returns all enabled transitions for a marking.
   *
   * @param marking marking
   * @return
   */
  def enabledTransitions(petriNet: PetriNet[P, T])(marking: M): Iterable[T]
}
