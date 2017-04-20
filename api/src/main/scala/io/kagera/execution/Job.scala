package io.kagera.execution

import ExceptionStrategy.RetryWithDelay
import io.kagera.api.colored.{ Marking, Transition, _ }

/**
 * A Job describes all the parameters that make a firing transition in a petri net.
 */
case class Job[S, E](
    id: Long,
    processState: S,
    transitionId: Long,
    consume: Marking,
    input: Any,
    failure: Option[ExceptionState] = None) {

  def isActive: Boolean = failure match {
    case Some(ExceptionState(_, _, _, RetryWithDelay(_))) ⇒ true
    case None                                             ⇒ true
    case _                                                ⇒ false
  }

  def failureCount = failure.map(_.failureCount).getOrElse(0)
}
