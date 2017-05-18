package io.kagera.akka.actor

import akka.persistence.{PersistentActor, RecoveryCompleted}
import io.kagera.api._
import io.kagera.api.colored.{Place, Transition}
import io.kagera.execution.EventSourcing._
import io.kagera.execution.{EventSourcing, Instance}
import io.kagera.persistence.{ObjectSerializer, Serialization, messages}

abstract class PetriNetInstanceRecovery[S](val topology: PetriNet[Place[_], Transition[_,_,_]], objectSerializer: ObjectSerializer) extends PersistentActor {

  implicit val system = context.system
  implicit val placeIdentifier: Identifiable[Place[_]] = colored.placeIdentifier
  implicit val transitionIdentifier: Identifiable[Transition[_,_,_]] = colored.transitionIdentifier

  val serializer = new Serialization[Place, Transition, S](objectSerializer)
  val eventSource = EventSourcing.apply[Place, Transition, S, Any](t => t.updateState.asInstanceOf[S => Any => S])

  def onRecoveryCompleted(state: Instance[Place, Transition, S])

  def persistEvent[T, E <: Event](instance: Instance[Place, Transition, S], e: E)(fn: E => T): Unit = {
    val serializedEvent = serializer.serializeEvent(e)(instance)
    persist(serializedEvent) { persisted => fn(e) }
  }

  private var recoveringState: Instance[Place, Transition, S] = Instance.uninitialized[Place, Transition, S](topology)

  private def applyToRecoveringState(e: AnyRef) = {
    val deserializedEvent = serializer.deserializeEvent(e)(recoveringState)
    recoveringState = eventSource(recoveringState)(deserializedEvent)
  }

  override def receiveRecover: Receive = {
    case e: messages.Initialized      ⇒ applyToRecoveringState(e)
    case e: messages.TransitionFired  ⇒ applyToRecoveringState(e)
    case e: messages.TransitionFailed ⇒ applyToRecoveringState(e)
    case RecoveryCompleted ⇒
      if (recoveringState.sequenceNr > 0)
        onRecoveryCompleted(recoveringState)
  }
}
