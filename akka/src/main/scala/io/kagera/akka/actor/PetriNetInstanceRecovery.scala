package io.kagera.akka.actor

import akka.persistence.{PersistentActor, RecoveryCompleted}
import io.kagera.api._
import io.kagera.runtime.EventSourcing._
import io.kagera.runtime.{EventSourcing, Instance}
import io.kagera.runtime.persistence.{ProtobufSerialization, messages}
import io.kagera.runtime.persistence.{ObjectSerializer, ProtobufSerialization}

abstract class PetriNetInstanceRecovery[P[_], T[_,_], S, E](
     val topology: PetriNet[P[_], T[_,_]],
     objectSerializer: ObjectSerializer,
     eventSourceFn: T[_,_] => (S => E => S)) extends PersistentActor {

  implicit val system = context.system
  implicit val placeIdentifier: Identifiable[P[_]]
  implicit val transitionIdentifier: Identifiable[T[_,_]]

  val eventSource = EventSourcing.apply[P, T, S, E](eventSourceFn)

  val serializer = new ProtobufSerialization[P, T, S](objectSerializer)

  def onRecoveryCompleted(state: Instance[P, T, S])

  def persistEvent[O, E <: Event](instance: Instance[P, T, S], e: E)(fn: E => O): Unit = {
    val serializedEvent = serializer.serializeEvent(e)(instance)
    persist(serializedEvent) { persisted => fn(e) }
  }

  private var recoveringState: Instance[P, T, S] = Instance.uninitialized[P, T, S](topology)

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
