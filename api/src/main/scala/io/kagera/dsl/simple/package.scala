//package io.kagera.dsl
//
//import io.kagera.api.PetriNet
//import io.kagera.execution.{ TransitionTask, TransitionTaskProvider }
//
//import scalax.collection.edge.WLDiEdge
//
//package object simple {
//
//  type Place[C] = Long
//  type Transition[I, O, S] = Long
//
//  def taskProvider[S]: TransitionTaskProvider[S, Place, Transition] = new TransitionTaskProvider[S, Place, Transition]() {
//    override def apply[Input, Output](petriNet: PetriNet[Place[_], Transition[_, _, _]], t: Transition[Input, Output, S]): TransitionTask[Place, Input, Output, S] =
//      (marking, input, state) ⇒ ???
//  }
//
//  /**
//   * Type alias for the node type of the scalax.collection.Graph backing the petri net.
//   */
//  type Node = Either[Place[_], Transition[_, _, _]]
//
//  /**
//   * Type alias for the edge type of the scalax.collection.Graph backing the petri net.
//   */
//  type Arc = WLDiEdge[Node]
//
//  implicit class TransitionDSL[Input, Output, State](t: Transition[Input, Output, State]) {
//    def ~>(p: Place[_], weight: Long = 1): Arc = arc(t, p, weight)
//  }
//
//  implicit class PlaceDSL[C](p: Place[C]) {
//    def ~>(t: Transition[_, _, _], weight: Long = 1, filter: C ⇒ Boolean = token ⇒ true): Arc = arc(p, t, weight, filter)
//  }
//
//  def arc(t: Transition[_, _, _], p: Place[_], weight: Long): Arc = WLDiEdge[Node, String](Right(t), Left(p))(weight, "")
//
//  def arc[C](p: Place[C], t: Transition[_, _, _], weight: Long, filter: C ⇒ Boolean = (token: C) ⇒ true): Arc = WLDiEdge[Node, String](Left(p), Right(t))(weight, "")
//
//}
