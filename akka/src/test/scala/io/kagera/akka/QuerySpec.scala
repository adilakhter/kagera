package io.kagera.akka

import java.util.UUID

import akka.persistence.inmemory.extension.{ InMemoryJournalStorage, StorageExtension }
import akka.persistence.query.PersistenceQuery
import akka.persistence.query.scaladsl.{ AllPersistenceIdsQuery, CurrentEventsByPersistenceIdQuery, CurrentPersistenceIdsQuery, ReadJournal }
import akka.stream.ActorMaterializer
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestProbe
import io.kagera.akka.actor.PetriNetInstanceProtocol.{ Initialize, Initialized, TransitionFired }
import io.kagera.akka.query.PetriNetQuery
import io.kagera.api.colored.dsl._
import io.kagera.api.colored.{ Marking, Place }
import io.kagera.execution.EventSourcing.{ InitializedEvent, TransitionFiredEvent }
import org.scalatest.BeforeAndAfterEach
import org.scalatest.Matchers._

import scala.collection.immutable._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class QuerySpec extends AkkaTestBase with BeforeAndAfterEach {

  val timeOut: Duration = 2 seconds

  implicit def materializer = ActorMaterializer()

  implicit def ec: ExecutionContext = system.dispatcher

  override protected def beforeEach(): Unit = {
    // Clean the journal before each test
    val tp = TestProbe()
    tp.send(StorageExtension(system).journalStorage, InMemoryJournalStorage.ClearJournal)
    tp.expectMsg(akka.actor.Status.Success(""))
  }

  "The query package" should {

    "return a source of events for a petriNet instance" in {

      val readJournal =
        PersistenceQuery(system).readJournalFor("inmemory-read-journal")
          .asInstanceOf[ReadJournal with CurrentEventsByPersistenceIdQuery]

      val p1 = Place[Unit](id = 1)
      val p2 = Place[Unit](id = 2)
      val p3 = Place[Unit](id = 3)
      val t1 = nullTransition[Unit](id = 1, automated = true)
      val t2 = nullTransition[Unit](id = 2, automated = true)

      val petriNet = createPetriNet(p1 ~> t1, t1 ~> p2, p2 ~> t2, t2 ~> p3)
      val processId = UUID.randomUUID().toString
      val instance = createPetriNetActor(petriNet, processId)

      instance ! Initialize(Marking(p1 -> 1))
      expectMsg(Initialized(Marking(p1 -> 1), ()))
      expectMsgPF(timeOut) { case TransitionFired(_, 1, _, _, _, _) ⇒ }
      expectMsgPF(timeOut) { case TransitionFired(_, 2, _, _, _, _) ⇒ }

      PetriNetQuery.eventsForInstance[Unit](processId = processId, topology = petriNet, readJournal = readJournal)
        .map(_._2) // Get the event from the tuple
        .runWith(TestSink.probe)
        .request(3)
        .expectNext(InitializedEvent(marking = Marking(p1 -> 1), state = ()))
        .expectNextChainingPF {
          case TransitionFiredEvent(_, transitionId, _, _, consumed, produced, _) ⇒
            transitionId shouldBe t1.id
            consumed shouldBe Marking(p1 -> 1)
            produced shouldBe Marking(p2 -> 1)
        }.expectNextChainingPF {
          case TransitionFiredEvent(_, transitionId, _, _, consumed, produced, _) ⇒
            transitionId shouldBe t2.id
            consumed shouldBe Marking(p2 -> 1)
            produced shouldBe Marking(p3 -> 1)
        }

    }

    "return all persisted processIds" in {

      val readJournal =
        PersistenceQuery(system).readJournalFor("inmemory-read-journal")
          .asInstanceOf[ReadJournal with AllPersistenceIdsQuery]

      // Setup petriNet and instances

      val p1 = Place[Unit](id = 1)
      val p2 = Place[Unit](id = 2)
      val p3 = Place[Unit](id = 3)
      val t1 = nullTransition[Unit](id = 1, automated = true)
      val t2 = nullTransition[Unit](id = 2, automated = true)
      val petriNet = createPetriNet(p1 ~> t1, t1 ~> p2, p2 ~> t2, t2 ~> p3)

      val processId1 = UUID.randomUUID().toString
      val instance1 = createPetriNetActor(petriNet, processId1)

      val processId2 = UUID.randomUUID().toString
      val instance2 = createPetriNetActor(petriNet, processId2)

      val processId3 = UUID.randomUUID().toString
      val instance3 = createPetriNetActor(petriNet, processId3)

      instance1 ! Initialize(Marking(p1 -> 1))
      instance2 ! Initialize(Marking(p1 -> 1))
      instance3 ! Initialize(Marking(p1 -> 1))

      // Setup is finished here, now continue with assertions

      PetriNetQuery.allProcessIds(readJournal)
        .runWith(TestSink.probe)
        .request(3)
        .expectNextUnorderedN(Seq(processId1, processId2, processId3))
        .expectNoMsg(1.second)
    }

    "return current persisted processIds, stream stopped in the end" in {

      val readJournal =
        PersistenceQuery(system).readJournalFor("inmemory-read-journal")
          .asInstanceOf[CurrentPersistenceIdsQuery]

      // Setup petriNet and instances

      val p1 = Place[Unit](id = 1)
      val p2 = Place[Unit](id = 2)
      val p3 = Place[Unit](id = 3)
      val t1 = nullTransition[Unit](id = 1, automated = true)
      val t2 = nullTransition[Unit](id = 2, automated = true)
      val petriNet = createPetriNet(p1 ~> t1, t1 ~> p2, p2 ~> t2, t2 ~> p3)

      val processId1 = UUID.randomUUID().toString
      val instance1 = createPetriNetActor(petriNet, processId1)

      val processId2 = UUID.randomUUID().toString
      val instance2 = createPetriNetActor(petriNet, processId2)

      val processId3 = UUID.randomUUID().toString
      val instance3 = createPetriNetActor(petriNet, processId3)

      instance1 ! Initialize(Marking(p1 -> 1))
      instance2 ! Initialize(Marking(p1 -> 1))
      instance3 ! Initialize(Marking(p1 -> 1))

      // Setup is finished here, now continue with assertions

      Thread.sleep(1000) // Wait here because otherwise we get an empty completed stream.

      PetriNetQuery.currentProcessIds(readJournal)
        .runWith(TestSink.probe)
        .request(3)
        .expectNextUnorderedN(Seq(processId1, processId2, processId3))
        .expectComplete()
    }

  }
}
