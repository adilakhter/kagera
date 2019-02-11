package io.kagera.api

object MultiSet {

  def empty[T]: MultiSet[T] = Map.empty[T, Int]

  def from[T](elements: Iterable[T]) = elements.foldLeft(empty[T]) { case (mset, e) ⇒ mset.multisetIncrement(e, 1) }

  def apply[T](elements: T*) = from(elements.toSeq)
}
