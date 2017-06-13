package io.kagera.akka

import akka.actor.Props
import akka.util.Timeout
import io.kagera.akka.actor.PetriNetInstance
import io.kagera.akka.actor.PetriNetInstanceProtocol._
import io.kagera.api._
import io.kagera.dsl.experiment._
import io.kagera.execution.PetriNetRuntime
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatest.Matchers._

class ExperimentSpec extends AkkaTestBase with ScalaFutures with MockitoSugar {

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

      val p1 = Place[Int](id= "p1")
      val p2 = Place[Int](id = "p2")
      val p3 = Place[Int](id = "p3")
      val p4 = Place[String](id = "p4")
      val p5 = Place[String](id = "p5")

      val t1 = Transition((x: Int) ⇒ (x + 100)-<>)
      val t2 = Transition((x: Int) ⇒ ((x * 2), "hello world"))

      val t3 = Transition((x: Int, y: String) ⇒ (x + "--" +"hello world")-<>)


      // Builder that allows building a Net structure and returns Seq of Arc
      val sequence =
        buildPetriNet(
          |>(p1) ~> t1 ~>> |>(p2),
          |>(p2) ~> t2 ~>> (p3, p4),
          (p3, p4) ~> t3 ~>> |> (p5)
        )

      val instance = system.actorOf(petriNetProps(sequence))
      val initialMarking = Marking(p1(1))

      import scala.concurrent.duration._

      implicit val timeout = Timeout(360 seconds)

      val result = instance ! Initialize(initialMarking)

      expectMsgClass(classOf[Initialized])
      val t1Fired = expectMsgClass(classOf[TransitionFired])
      val t2Fired = expectMsgClass(classOf[TransitionFired])
      val t3Fired = expectMsgClass(classOf[TransitionFired])

      val endMarking2 = Marking.unmarshal(t2Fired.result.marking, sequence.places.getById)
      val endMarking3 = Marking.unmarshal(t3Fired.result.marking, sequence.places.getById)

      endMarking2 shouldBe Marking(p3(202), p4("hello world"))
      endMarking3 shouldBe Marking(p5("202--hello world"))
    }
  }
}
