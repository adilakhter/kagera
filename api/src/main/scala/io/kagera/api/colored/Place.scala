package io.kagera.api.colored

import io.kagera.api.{MarkedPlace, MultiSet}

object Place {
  def apply[Color](id: Long): Place[Color] = Place(id, s"p$id")
}

/**
 * A Place in a colored petri net.
 */
case class Place[Color](val id: Long, val label: String) {

  def apply[T <: Color](_tokens: T*): MarkedPlace[Place, Color] = (this, MultiSet(_tokens: _*))
}