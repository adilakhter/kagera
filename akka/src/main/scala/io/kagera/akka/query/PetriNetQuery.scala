package io.kagera.akka.query

import akka.NotUsed
import akka.actor.ActorSystem
import akka.persistence.query.scaladsl._
import akka.stream.scaladsl._
import io.kagera.akka.actor.{ AkkaObjectSerializer, PetriNetInstance }
import io.kagera.api.colored.ExecutablePetriNet
import io.kagera.execution.EventSourcing._
import io.kagera.execution._
import io.kagera.persistence.Encryption.NoEncryption
import io.kagera.persistence.{ Encryption, Serialization }

object PetriNetQuery {

  def eventsForInstance[S](processId: String,
    topology: ExecutablePetriNet[S],
    encryption: Encryption = NoEncryption,
    readJournal: CurrentEventsByPersistenceIdQuery)(implicit actorSystem: ActorSystem): Source[(Instance[S], Event), NotUsed] = {

    val serializer = new Serialization(new AkkaObjectSerializer(actorSystem, encryption))

    val persistentId = PetriNetInstance.processId2PersistenceId(processId)
    val src = readJournal.currentEventsByPersistenceId(persistentId, 0, Long.MaxValue)

    src.scan[(Instance[S], Event)]((Instance.uninitialized(topology), null.asInstanceOf[Event])) {
      case ((instance, prev), e) ⇒
        val serializedEvent = e.event.asInstanceOf[AnyRef]
        val deserializedEvent = serializer.deserializeEvent(serializedEvent)(instance)
        val updatedInstance = EventSourcing.apply(instance)(deserializedEvent)
        (updatedInstance, deserializedEvent)
    }.drop(1) // Just to drop the first event 'uninitialized', not interesting for the consumers.
  }

  def allProcessIds(readJournal: AllPersistenceIdsQuery)(implicit actorSystem: ActorSystem): Source[String, NotUsed] = {
    readJournal.allPersistenceIds
      .map(PetriNetInstance.persistenceId2ProcessId) // This filters out anything that is not a processId (like shard actors, any other actors)
      .collect {
        case Some(processId) ⇒ processId
      }
  }

  def currentProcessIds(readJournal: CurrentPersistenceIdsQuery)(implicit actorSystem: ActorSystem): Source[String, NotUsed] = {
    readJournal.currentPersistenceIds()
      .map(PetriNetInstance.persistenceId2ProcessId) // This filters out anything that is not a processId (like shard actors, any other actors)
      .collect {
        case Some(processId) ⇒ processId
      }
  }

}