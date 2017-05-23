package io.kagera.akka.actor

import akka.persistence.{PersistentActor, RecoveryCompleted}
import io.kagera.api._
import io.kagera.execution.EventSourcing._
import io.kagera.execution.{EventSourcing, Instance}
import io.kagera.persistence.{ObjectSerializer, Serialization, messages}

abstract class PetriNetInstanceRecovery[P[_], T[_,_,_], S](
     val topology: PetriNet[P[_], T[_,_,_]],
     objectSerializer: ObjectSerializer,
     eventSourceFn: T[_,_,_] => (S => Any => S)) extends PersistentActor {

  implicit val system = context.system
  implicit val placeIdentifier: Identifiable[P[_]]
  implicit val transitionIdentifier: Identifiable[T[_,_,_]]

  val eventSource = EventSourcing.apply[P, T, S, Any](eventSourceFn)

  val serializer = new Serialization[P, T, S](objectSerializer)

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
