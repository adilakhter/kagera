package io.kagera.dsl

import io.kagera.api.{MarkedPlace, Marking, MultiSet}
import io.kagera.dsl.colored.Place
import io.kagera.dsl.experiment.dslExperiment.TransformationArc
import shapeless._
import shapeless.ops.function.FnToProduct

import scalax.collection.edge.WLDiEdge

package object experiment {

  case class RuntimeTransition[I, O](fn: Marking[Place] => Marking[Place])

  /**
    * Type alias for the node type of the scalax.collection.Graph backing the petri net.
    */
  type Node = Either[Place[_], RuntimeTransition[_, _]]

  /**
    * Type alias for the edge type of the scalax.collection.Graph backing the petri net.
    */
  type Arc = WLDiEdge[Node]

  def arc(t: RuntimeTransition[_, _], p: Place[_]): Arc = WLDiEdge[Node, String](Right(t), Left(p))(1, "")

  def arc[C](p: Place[C], t: RuntimeTransition[_, _]): Arc = WLDiEdge[Node, String](Left(p), Right(t))(1, "")

  case class Place[A](id: String) extends AnyVal {
    def apply[T <: A](_tokens: T*): MarkedPlace[Place, A] = (this, MultiSet(_tokens: _*))
  }

  case class Transition[F](fn: F) extends AnyVal

  def |>[A](p: Place[A]): Tuple1[Place[A]] = Tuple1(p)

  def |>[F, R<: Product, C<: HList, I <: HList, O <: HList](t: Transition[F])(implicit fp: FnToProduct.Aux[F, C ⇒ R]) = TransformationArc(transition = t)


  def hlistToPlaceList(hlist: HList): List[Place[_]] = hlist match {
    case HNil ⇒ List.empty
    case x :: xs ⇒ List(x.asInstanceOf[Place[_]]) ++ hlistToPlaceList(xs)
  }

  import shapeless.ops.hlist.HKernelAux

  def intLen[T <: HList](implicit ker: HKernelAux[T]): Int = ker().length

  def zipHLs(l1: HList, l2: HList): HList = (l1, l2) match {
    case (h1 :: t1, h2 :: t2) => (h1, h2) :: zipHLs(t1, t2)
    case _ => HNil
  }



}
