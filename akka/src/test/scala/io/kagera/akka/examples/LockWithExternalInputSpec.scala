package io.kagera.akka.examples

import akka.actor.Props
import akka.pattern._
import akka.util.Timeout
import io.kagera.akka.AkkaTestBase
import io.kagera.akka.actor.PetriNetInstance
import io.kagera.akka.actor.PetriNetInstanceProtocol._
import io.kagera.api._
import io.kagera.dsl.experiment._
import org.scalatest.Matchers._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar

import scala.concurrent.{Await, ExecutionContext}

class LockWithExternalInputSpec extends AkkaTestBase with ScalaFutures with MockitoSugar {

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
      val endMarking1 = Marking.unmarshal(t0Fired.result.marking, sequence.places.getById)

      val expectedEndmarking = Marking(p13(999999))
      //      endMarking2 shouldBe Marking(p3(202), p4("hello world"))
      endMarking1 shouldBe expectedEndmarking

      val futureMarking = (instance ? GetState).mapTo[InstanceState].map(state ⇒ Marking.unmarshal(state.marking, sequence.places.getById))

      Await.result(futureMarking, timeout.duration) shouldBe expectedEndmarking
    }
  }
}
