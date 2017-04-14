package io.kagera

import java.io.{ PrintWriter, StringWriter }

import cats.data.State
import fs2.Task
import io.kagera.api._
import io.kagera.api.colored._
import io.kagera.execution.EventSourcing._

package object execution {

  /**
   * An exception handler function associated with a transition.
   */
  type TransitionExceptionHandler = (Throwable, Int) ⇒ ExceptionStrategy
}
