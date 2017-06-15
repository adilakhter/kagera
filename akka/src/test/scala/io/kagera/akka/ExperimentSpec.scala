package io.kagera.akka

import akka.actor.Props
import akka.util.Timeout
import akka.pattern.ask
import io.kagera.akka.actor.PetriNetInstance
import io.kagera.akka.actor.PetriNetInstanceProtocol._
import io.kagera.api._
import io.kagera.dsl.experiment._
import io.kagera.execution.PetriNetRuntime
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatest.Matchers._

import scala.concurrent.{ Await, ExecutionContext }

class ExperimentSpec extends AkkaTestBase with ScalaFutures with MockitoSugar {

  implicit val ec: ExecutionContext = system.dispatcher

  def petriNetProps(topology: PetriNet[Place[_], RuntimeTransition[_, _]]) =
    Props(new PetriNetInstance[Place, RuntimeTransition, Unit, Unit](
      topology,
      instanceSettings,
      petriNetRuntime,
      placeId _,
      transitionId _)
    )

  "A petri net actor" should {

    "execute a sequence" in {

      val p1 = Place[Int](id = "p1")
      val p2 = Place[Int](id = "p2")
      val p3 = Place[Int](id = "p3")
      val p4 = Place[String](id = "p4")
      val p5 = Place[String](id = "p5")

      val t0 = Transition(() ⇒ 1-<>)
      val t1 = Transition((x: Int) ⇒ (x + 100)-<>)
      val t2 = Transition((x: Int) ⇒ ((x * 2), "hello world"))
      val t3 = Transition((x: Int, y: String) ⇒ (x + "--" + "hello world")-<>)

      // Builder that allows building a Net structure and returns Seq of Arc
      val sequence =
        buildPetriNet(
          |>(t0) ~>> |>(p1),
          |>(p1) ~> t1 ~>> |>(p2),
          |>(p2) ~> t2 ~>> (p3, p4),
          (p3, p4) ~> t3 ~>> |>(p5)
        )

      val instance = system.actorOf(petriNetProps(sequence))
      val initialMarking = Marking.empty[Place]

      import scala.concurrent.duration._

      implicit val timeout = Timeout(360 seconds)

      val result = instance ! Initialize(initialMarking)
      expectMsgClass(classOf[Initialized])

      instance ! FireTransition(t0.id, 1-<>)

      val t0Fired = expectMsgClass(classOf[TransitionFired])
      val t1Fired = expectMsgClass(classOf[TransitionFired])
      val t2Fired = expectMsgClass(classOf[TransitionFired])
      val t3Fired = expectMsgClass(classOf[TransitionFired])

      //      val endMarking2 = Marking.unmarshal(t3Fired.result.marking, sequence.places.getById)
      val endMarking3 = unmarshal(t3Fired.result.marking, sequence.places.getById)

      //      endMarking2 shouldBe Marking(p3(202), p4("hello world"))
      endMarking3 shouldBe Marking(p5("202--hello world"))
    }

    "model a lock and execute it" in {

      val p11 = Place[Int](id = "p11")
      val p12 = Place[Int](id = "p12")
      val p13 = Place[Int](id = "p13")

      type Lock = Int
      val lock = Place[Lock](id = "lock")
      val lockValue = 1

      val p21 = Place[String](id = "p21")
      val p22 = Place[String](id = "p22")
      val p23 = Place[String](id = "p23")

      val t1BeginCritialSection = Transition((x: Int, lock: Lock) ⇒ (x * 100)-<>)
      val t1EndCritialSection = Transition((result: Int) ⇒ (result, lockValue))

      val t2BeginCritialSection = Transition((x: String, lock: Lock) ⇒ (x.capitalize)-<>)
      val t2EndCritialSection = Transition((result: String) ⇒ (result, lockValue))

      // Builder that allows building a Net structure and returns Seq of Arc
      val sequence =
        buildPetriNet(
          (p11, lock) ~> t1BeginCritialSection ~>> |>(p12),
          |>(p12) ~> t1EndCritialSection ~>> (p13, lock),
          (p21, lock) ~> t2BeginCritialSection ~>> |>(p22),
          |>(p22) ~> t2EndCritialSection ~>> (p23, lock)
        )

      val instance = system.actorOf(petriNetProps(sequence))
      val initialMarking = Marking(p11(99), p21("abcdefg"), lock(lockValue))

      import scala.concurrent.duration._
      implicit val timeout = Timeout(360 seconds)

      val result = instance ! Initialize(initialMarking)
      expectMsgClass(classOf[Initialized])

      val t0Fired = expectMsgClass(classOf[TransitionFired])
      val t1Fired = expectMsgClass(classOf[TransitionFired])
      val t2Fired = expectMsgClass(classOf[TransitionFired])
      val t3Fired = expectMsgClass(classOf[TransitionFired])

      //      val endMarking2 = Marking.unmarshal(t3Fired.result.marking, sequence.places.getById)
      val endMarking3 = unmarshal(t3Fired.result.marking, sequence.places.getById)

      //      endMarking2 shouldBe Marking(p3(202), p4("hello world"))
      endMarking3 shouldBe Marking(lock(lockValue), p13(9900), p23("abcdefg".capitalize))
    }

    "model a lock and sensory input and execute it" in {

      val p11 = Place[Int](id = "p11")
      val p12 = Place[Int](id = "p12")
      val p13 = Place[Int](id = "p13")

      val externalInput = Place[Int](id = "externalInput")

      val provideExternalInput = Transition(() ⇒ (1)-<>)
      val t1BeginCritialSection = Transition((x: Int) ⇒ (x * 100)-<>)
      val t1EndCritialSection = Transition((result: Int, externalInput: Int) ⇒ externalInput-<>)

      // Builder that allows building a Net structure and returns Seq of Arc
      val sequence =
        buildPetriNet(
          |>(provideExternalInput) ~>> |>(externalInput),
          |>(p11) ~> t1BeginCritialSection ~>> |>(p12),
          (p12, externalInput) ~> t1EndCritialSection ~>> |>(p13)
        )

      val instance = system.actorOf(petriNetProps(sequence))
      val initialMarking = Marking(p11(99))

      import scala.concurrent.duration._
      implicit val timeout = Timeout(360 seconds)

      val result = instance ! Initialize(initialMarking)
      expectMsgClass(classOf[Initialized])
      expectMsgClass(classOf[TransitionFired])

      instance ! FireTransition(provideExternalInput.id, 999999-<>)

      expectMsgClass(classOf[TransitionFired])
      val t0Fired = expectMsgClass(classOf[TransitionFired])

      //      val endMarking2 = Marking.unmarshal(t3Fired.result.marking, sequence.places.getById)
      val endMarking1 = unmarshal(t0Fired.result.marking, sequence.places.getById)

      val expectedEndmarking = Marking(p13(999999))
      //      endMarking2 shouldBe Marking(p3(202), p4("hello world"))
      endMarking1 shouldBe expectedEndmarking

      val futureMarking = (instance ? GetState).mapTo[InstanceState].map(state ⇒ unmarshal(state.marking, sequence.places.getById))

      Await.result(futureMarking, timeout.duration) shouldBe expectedEndmarking
    }
  }
}
