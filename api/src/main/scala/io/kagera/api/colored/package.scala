package io.kagera.api

import io.kagera.api.multiset._
import scalax.collection.edge.WLDiEdge

package object colored {

  /**
   * Type alias for the node type of the scalax.collection.Graph backing the petri net.
   */
  type Node = Either[Place[_], Transition[_, _, _]]

  /**
   * Type alias for the edge type of the scalax.collection.Graph backing the petri net.
   */
  type Arc = WLDiEdge[Node]

  /**
   * Type alias for a single marked place, meaning a place containing tokens.
   *
   * @tparam Color the color of the place.
   */
  type MarkedPlace[P[_], Color] = (P[Color], MultiSet[Color])

  /**
   * Type alias for a colored petri net.
   */
  type ColoredPetriNet = PetriNet[Place[_], Transition[_, _, _]]

  /**
   * Type alias for a marking.
   */
  type Marking[P[_]] = HMap[P, MultiSet]

  /**
   * Type alias for marking data.
   */
  type MarkingData = Map[Long, MultiSet[_]]

  /**
   * Some convenience method additions to work with Markings.
   */
  implicit class MarkingAdditions[P[_]](marking: Marking[P]) {

    // Note: extra .map(identity) is a needed to workaround the scala Map serialization bug: https://issues.scala-lang.org/browse/SI-7005
    def multiplicities: MultiSet[P[_]] = marking.data.mapValues(_.multisetSize).map(identity)

    def add[C](p: P[C], value: C, count: Int = 1): Marking[P] = {
      val newTokens = marking.getOrElse(p, MultiSet.empty).multisetIncrement(value, count)
      marking.+(p -> newTokens)
    }

    def |-|(other: Marking[P]): Marking[P] = other.keySet.foldLeft(marking) {

      case (result, place) ⇒
        val p = place.asInstanceOf[P[Any]]
        marking.get(p) match {
          case None ⇒ result
          case Some(tokens) ⇒
            val newTokens = tokens.multisetDifference(other(p))
            if (newTokens.isEmpty)
              result - p
            else
              result + (p -> newTokens)
        }
    }

    def |+|(other: Marking[P]): Marking[P] = other.keySet.foldLeft(marking) {
      case (result, place) ⇒
        val p = place.asInstanceOf[P[Any]]
        val newTokens = marking.get(p) match {
          case None         ⇒ other(p)
          case Some(tokens) ⇒ tokens.multisetSum(other(p))
        }

        result + (p -> newTokens)
    }
  }

  /**
   * TODO
   *
   * Incorporate the upper bounds of type parameters into this type
   *
   * Place color:       C
   * Transition input:  I
   * Transition output: E
   *
   * That way we can define different types of Petri nets:
   *
   * Uncolored petri nets, where C is Unit
   * Non interactive nets, Where I is Unit
   * Nets without State & Event sourcing, Where S & E are Unit
   *
   * @tparam S The 'global' state transitions close over
   */
  type ExecutablePetriNet[S] = ColoredPetriNet

  implicit def toMarkedPlace[P[_]](tuple: (P[Unit], Int)): MarkedPlace[P, Unit] = tuple._1 -> Map[Unit, Int](() -> tuple._2)

  implicit class IterableToMarking[P[_]](i: Iterable[(P[_], MultiSet[_])]) {
    def toMarking: Marking[P] = HMap[P, MultiSet](i.toMap[P[_], MultiSet[_]])
  }

  implicit class MultiSetToMarking[P[_]](m: MultiSet[P[_]]) {
    def toMarking: Marking[P] = m.map { case (p, n) ⇒ p -> Map(() -> n) }.toMarking
  }

  //  implicit def toMarking[P[_]](m: MultiSet[P[_]]): Marking[P] = m.toMarking

  implicit class ColoredPetriNetAdditions(petriNet: ColoredPetriNet) {
    def getEdge(p: Place[_], t: Transition[_, _, _]): Option[PTEdge[Any]] = petriNet.innerGraph.findPTEdge(p, t).map(_.label.asInstanceOf[PTEdge[Any]])
  }

  implicit def placeLabel[C](p: Place[C]): Label = Label(p.label)

  implicit def placeIdentifier(p: Place[_]): Id = Id(p.id)

  implicit def transitionLabeler(t: Transition[_, _, _]): Label = Label(t.label)

  implicit def transitionIdentifier(t: Transition[_, _, _]): Id = Id(t.id)
}
