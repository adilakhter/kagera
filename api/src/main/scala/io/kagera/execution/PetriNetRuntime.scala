package io.kagera.execution

import io.kagera.api.{ Marking, TokenGame }
import io.kagera.execution.ExceptionStrategy.BlockTransition

/**
 * Encapsulates all components required to 'run' a petri net instance
 *
 * @tparam P The place type
 * @tparam T The transition type
 * @tparam S The state type
 * @tparam E The event type
 */
trait PetriNetRuntime[P[_], T[_, _], S, E] {

  val tokenGame: TokenGame[P[_], T[_, _], Marking[P]]

  val eventSourceFn: T[_, _] ⇒ (S ⇒ E ⇒ S) = t ⇒ (s ⇒ e ⇒ s)

  val exceptionHandlerFn: T[_, _] ⇒ TransitionExceptionHandler = t ⇒ ((e, n) ⇒ BlockTransition)

  val taskProvider: TransitionTaskProvider[S, P, T]

  lazy val jobPicker = new JobPicker[P, T](tokenGame)

  lazy val jobExecutor: JobExecutor[S, P, T] = new JobExecutor[S, P, T](taskProvider, exceptionHandlerFn)
}
