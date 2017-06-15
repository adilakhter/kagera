package io.kagera.akka.examples

import akka.actor.Props
import akka.util.Timeout
import io.kagera.akka.AkkaTestBase
import io.kagera.akka.actor.PetriNetInstance
import io.kagera.akka.actor.PetriNetInstanceProtocol._
import io.kagera.api._
import io.kagera.dsl.experiment._
import org.scalatest.Matchers._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar

import scala.concurrent.{ Await, ExecutionContext }

class SimplePetriNetSpec extends AkkaTestBase with ScalaFutures with MockitoSugar {

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

      val t0 = Transition(() ⇒ 1 -<>)
      val t1 = Transition((x: Int) ⇒ (x + 100) -<>)
      val t2 = Transition((x: Int) ⇒ ((x * 2), "hello world"))
      val t3 = Transition((x: Int, y: String) ⇒ (x + "--" + "hello world") -<>)
      val t4 = Transition((y: String) ⇒ {
        println(s"result of the computation is $y");
        () -<>
      })

      // Builder that allows building a Net structure and returns Seq of Arc
      val sequence =
        buildPetriNet(
          |>(t0) ~>> |>(p1),
          |>(p1) ~> t1 ~>> |>(p2),
          |>(p2) ~> t2 ~>> (p3, p4),
          (p3, p4) ~> t3 ~>> |>(p5),
          |>(p5) ~> t4

        )

      val instance = system.actorOf(petriNetProps(sequence))
      val initialMarking = Marking.empty[Place]

      import scala.concurrent.duration._

      implicit val timeout = Timeout(360 seconds)

      val result = instance ! Initialize(initialMarking)
      expectMsgClass(classOf[Initialized])

      instance ! FireTransition(t0.id, 1 -<>)

      val t0Fired = expectMsgClass(classOf[TransitionFired])
      val t1Fired = expectMsgClass(classOf[TransitionFired])
      val t2Fired = expectMsgClass(classOf[TransitionFired])
      val t3Fired = expectMsgClass(classOf[TransitionFired])

      //      val endMarking2 = Marking.unmarshal(t3Fired.result.marking, sequence.places.getById)
      val endMarking3 = unmarshal(t3Fired.result.marking, sequence.places.getById)

      //      endMarking2 shouldBe Marking(p3(202), p4("hello world"))
      endMarking3 shouldBe Marking(p5("202--hello world"))

      instance ! FireTransition(t0.id, 1090 -<>)
    }
  }
}
