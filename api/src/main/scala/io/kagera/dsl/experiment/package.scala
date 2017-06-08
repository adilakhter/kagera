package io.kagera.dsl

import io.kagera.api.{MarkedPlace, MultiSet}
import io.kagera.dsl.colored.Place
import io.kagera.dsl.experiment.dslExperiment.TransformationArc
import shapeless._

package object experiment {

  case class Place[A](id: String) extends AnyVal {
    def apply[T <: A](_tokens: T*): MarkedPlace[Place, A] = (this, MultiSet(_tokens: _*))
  }

  case class Transition[F](fn: F) extends AnyVal

  def |>[A](p: Place[A]): Tuple1[Place[A]] = Tuple1(p)

  def |>[F, I <: HList, O <: HList](t: Transition[F]) = TransformationArc(transition = t)


  def hlistToPlaceList(hlist: HList): List[Place[_]] = hlist match {
    case HNil ⇒ List.empty
    case x :: xs ⇒ List(x.asInstanceOf[Place[_]]) ++ hlistToPlaceList(xs)
  }
}
