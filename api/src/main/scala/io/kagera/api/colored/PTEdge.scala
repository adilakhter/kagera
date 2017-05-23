package io.kagera.api.colored

/**
 * An edge from a place to a transition.
 */
case class PTEdge[T](weight: Long, filter: T ⇒ Boolean)
