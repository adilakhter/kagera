package io.kagera.dsl.colored

import fs2.Task
import io.kagera.api._
import io.kagera.execution.ExceptionStrategy.BlockTransition
import io.kagera.execution._

import scala.util.Random

trait StateTransitionNet[S, E] {

  def eventSourcefunction: S ⇒ E ⇒ S

  def eventTaskProvider: TransitionTaskProvider[S, Place, Transition] = new TransitionTaskProvider[S, Place, Transition] {
    override def apply[Input, Output](petriNet: PetriNet[Place[_], Transition[_, _]], t: Transition[Input, Output]): TransitionTask[Place, Input, Output, S] =
      (marking, state, input) ⇒ {
        val eventTask = t.asInstanceOf[StateTransition[S, Output]].produceEvent(state)
        val produceMarking: Marking[Place] = toMarking[Place](petriNet.outMarking(t))
        eventTask.map(e ⇒ (produceMarking, e))
      }
  }

  val runtime: PetriNetRuntime[Place, Transition, S, Any] = new PetriNetRuntime[Place, Transition, S, Any] {
    override val eventSourceFn: (Transition[_, _]) ⇒ (S) ⇒ (Any) ⇒ S = t ⇒ eventSourcefunction.asInstanceOf[S ⇒ Any ⇒ S]
    override val tokenGame: TokenGame[Place[_], Transition[_, _], Marking[Place]] = new ReferenceTokenGame[Place, Transition]
    override val taskProvider: TransitionTaskProvider[S, Place, Transition] = eventTaskProvider
    override val exceptionHandlerFn = (t: Transition[_, _]) ⇒ t.exceptionStrategy
    override val jobPicker = new JobPicker[Place, Transition](tokenGame) {
      override def isFireable[S](instance: Instance[Place, Transition, S], t: Transition[_, _]): Boolean =
        t.isAutomated && !instance.isBlockedReason(t).isDefined
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
