package io.kagera.demo

import akka.NotUsed
import akka.actor.Actor
import akka.persistence.cassandra.query.scaladsl.CassandraReadJournal
import akka.persistence.query.PersistenceQuery
import akka.stream.scaladsl.Source
import io.kagera.akka.persistence.TransitionFired
import io.kagera.dsl.colored._
import io.kagera.api.multiset._

trait Queries {

  this: ConfiguredActorSystem ⇒

  // obtain read journal
  val readJournal = PersistenceQuery(system).readJournalFor[CassandraReadJournal](CassandraReadJournal.Identifier)

  def allProcessIds: Source[String, NotUsed] = readJournal.allPersistenceIds()

  def journalFor(id: String): Source[String, NotUsed] = {
    readJournal.currentEventsByPersistenceId(s"process-$id", 0, Long.MaxValue).map {
      e ⇒ e.event.toString
    }
  }

}

class AggregateMarking[S](topology: ExecutablePetriNet[S]) extends Actor {

  override def receive: Receive = updateMarking(MultiSet.empty)

  def updateMarking(aggregateMarking: MultiSet[Long]): Receive = {

    case TransitionFired(_, Some(tid), Some(started), Some(completed), consumed, produced, data) ⇒
      val minusConsumed = consumed.foldLeft(aggregateMarking) {
        case (aggregate, token) ⇒ aggregate.multisetDecrement(token.placeId.get, token.count.get)
      }

      val newAggregate = produced.foldLeft(minusConsumed) {
        case (aggregate, token) ⇒ aggregate.multisetIncrement(token.placeId.get, token.count.get)
      }

      context become updateMarking(newAggregate)
  }
}
