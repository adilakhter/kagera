package io.kagera.runtime

import io.kagera.api.Marking
import io.kagera.runtime.ExceptionStrategy.RetryWithDelay

/**
 * A Job encapsulates all the parameters that make a firing transition in a petri net.
 */
case class Job[P[_], T[_, _], S, E](
    id: Long,
    processState: S,
    transition: T[_, E],
    consume: Marking[P],
    input: Any,
    failure: Option[ExceptionState] = None) {

  def isActive: Boolean = failure match {
    case Some(ExceptionState(_, _, _, RetryWithDelay(_))) ⇒ true
    case None                                             ⇒ true
    case _                                                ⇒ false
  }

  def failureCount = failure.map(_.failureCount).getOrElse(0)
}
