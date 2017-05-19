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
   * Type alias for a colored petri net.
   */
  type ColoredPetriNet = PetriNet[Place[_], Transition[_, _, _]]

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

  //  implicit def toMarking[P[_]](m: MultiSet[P[_]]): Marking[P] = m.toMarking

  implicit class ColoredPetriNetAdditions(petriNet: ColoredPetriNet) {
    def getEdge(p: Place[_], t: Transition[_, _, _]): Option[PTEdge[Any]] = petriNet.innerGraph.findPTEdge(p, t).map(_.label.asInstanceOf[PTEdge[Any]])
  }

  implicit def placeLabel[C](p: Place[C]): Label = Label(p.label)

  implicit def placeIdentifier(p: Place[_]): Id = Id(p.id)

  implicit def transitionLabeler(t: Transition[_, _, _]): Label = Label(t.label)

  implicit def transitionIdentifier(t: Transition[_, _, _]): Id = Id(t.id)
}
