package io.kagera.dsl

import fs2.Task
import io.kagera.api._
import io.kagera.dsl.colored.transitions.{ AbstractTransition, IdentityTransition }
import io.kagera.execution._

import scalax.collection.edge.WLDiEdge
import scalax.collection.immutable.Graph

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

  implicit class ColoredPetriNetAdditions(petriNet: ColoredPetriNet) {
    def getEdge(p: Place[_], t: Transition[_, _, _]): Option[PTEdge[Any]] = petriNet.innerGraph.findPTEdge(p, t).map(_.label.asInstanceOf[PTEdge[Any]])
  }

  val jobPicker = new JobPicker[Place, Transition](new ColoredTokenGame()) {
    override def isFireable[S](instance: Instance[Place, Transition, S], t: Transition[_, _, _]): Boolean =
      t.isAutomated && !instance.isBlockedReason(t).isDefined
  }

  def taskProvider[S] = new TransitionTaskProvider[S, Place, Transition] {
    override def apply[Input, Output](petriNet: PetriNet[Place[_], Transition[_, _, _]], t: Transition[Input, Output, S]): TransitionTask[Place, Input, Output, S] =
      t.apply(petriNet.inMarking(t), petriNet.outMarking(t))
  }

  implicit def placeLabel[C](p: Place[C]): Label = Label(p.label)

  implicit def placeIdentifier(p: Place[_]): Id = Id(p.id)

  implicit def transitionLabeler(t: Transition[_, _, _]): Label = Label(t.label)

  implicit def transitionIdentifier(t: Transition[_, _, _]): Id = Id(t.id)

  implicit class TransitionDSL[Input, Output, State](t: Transition[Input, Output, State]) {
    def ~>(p: Place[_], weight: Long = 1): Arc = arc(t, p, weight)
  }

  implicit class PlaceDSL[C](p: Place[C]) {
    def ~>(t: Transition[_, _, _], weight: Long = 1, filter: C ⇒ Boolean = token ⇒ true): Arc = arc(p, t, weight, filter)
  }

  def arc(t: Transition[_, _, _], p: Place[_], weight: Long): Arc = WLDiEdge[Node, String](Right(t), Left(p))(weight, "")

  def arc[C](p: Place[C], t: Transition[_, _, _], weight: Long, filter: C ⇒ Boolean = (token: C) ⇒ true): Arc = {
    val innerEdge = new PTEdge[C](weight, filter)
    WLDiEdge[Node, PTEdge[C]](Left(p), Right(t))(weight, innerEdge)
  }

  def constantTransition[I, O, S](id: Long, label: Option[String] = None, automated: Boolean = false, constant: O) =
    new AbstractTransition[I, O, S](id, label.getOrElse(s"t$id"), automated) with IdentityTransition[I, O, S] {

      override val toString = label

      override def apply(inAdjacent: MultiSet[Place[_]], outAdjacent: MultiSet[Place[_]]) =
        (marking, state, input) ⇒ Task.delay {
          val produced = outAdjacent.map {
            case (place, weight) ⇒ place -> Map(constant -> weight)
          }.toMarking

          (produced, constant)
        }
    }

  def nullTransition[S](id: Long, label: Option[String] = None, automated: Boolean = false): Transition[Unit, Unit, S] =
    constantTransition[Unit, Unit, S](id, label, automated, ())

  def createPetriNet[S](params: Arc*): ColoredPetriNet = {
    val petriNet = new ScalaGraphPetriNet(Graph(params: _*))

    requireUniqueElements(petriNet.places.toSeq.map(_.id), "Place identifier")
    requireUniqueElements(petriNet.transitions.toSeq.map(_.id), "Transition identifier")

    petriNet
  }
}
