package io.kagera.dsl.colored

import fs2.Task
import io.kagera.api._
import io.kagera.execution.ExceptionStrategy.BlockTransition
import io.kagera.execution.{ TransitionExceptionHandler, TransitionTask, TransitionTaskProvider }

import scala.util.Random

trait StateTransitionNet[S, E] {

  def eventSourceFn: S ⇒ E ⇒ S

  def taskProvider: TransitionTaskProvider[S, Place, Transition] = new TransitionTaskProvider[S, Place, Transition] {
    override def apply[Input, Output](petriNet: PetriNet[Place[_], Transition[_, _]], t: Transition[Input, Output]): TransitionTask[Place, Input, Output, S] =
      (marking, state, input) ⇒ {
        val eventTask = t.asInstanceOf[StateTransition[S, Output]].produceEvent(state)
        val produceMarking: Marking[Place] = toMarking[Place](petriNet.outMarking(t))
        eventTask.map(e ⇒ (produceMarking, e))
      }
  }

  def stateTransition(id: Long = Math.abs(Random.nextLong), label: Option[String] = None, automated: Boolean = false,
    exceptionStrategy: TransitionExceptionHandler = (e, n) ⇒ BlockTransition)(fn: S ⇒ E): Transition[Unit, E] =
    StateTransition(id, label.getOrElse(s"t$id"), automated, exceptionStrategy, (s: S) ⇒ Task.now(fn(s)))

  def constantTransition[I, O](id: Long, label: Option[String] = None, automated: Boolean = false, constant: O) =
    StateTransition[I, O](id, label.getOrElse(s"t$id"), automated, (e, n) ⇒ BlockTransition, s ⇒ Task.now(constant))

  def nullTransition[S](id: Long, label: Option[String] = None, automated: Boolean = false): Transition[Unit, Unit] =
    constantTransition[Unit, Unit](id, label, automated, ())
}
