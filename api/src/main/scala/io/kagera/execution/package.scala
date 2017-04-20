package io.kagera

import fs2.Task
import io.kagera.api.colored._

package object execution {

  /**
   * An exception handler function associated with a transition.
   */
  type TransitionExceptionHandler = (Throwable, Int) ⇒ ExceptionStrategy

  /**
   * An (asynchronous) function associated with a transition
   *
   * @tparam Input  The input delivered to the transition from outside the process.
   * @tparam Output The output emitted by the transition.
   * @tparam State  The state the transition closes over.
   */
  type TransitionTask[Input, Output, State] = (Marking, State, Input) ⇒ Task[(Marking, Output)]
}
