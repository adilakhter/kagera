package io.kagera.akka

import akka.actor.Props
import io.kagera.akka.actor.PetriNetInstance
import io.kagera.akka.actor.PetriNetInstanceProtocol._
import io.kagera.api.Marking
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import io.kagera.dsl.experiment._
import io.kagera.execution.PetriNetRuntime
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await

class ExperimentSpec extends AkkaTestBase with ScalaFutures with MockitoSugar {

  "A petri net actor" should {

    "do something" in {

      val p1 = Place[Int](id = "t1")
      val p2 = Place[Int](id = "2")
      val p3 = Place[Int](id = "3")

      val p4 = Place[String](id = "string1")
      val p5 = Place[String](id = "string2")

      val tr0 = Transition(() ⇒ 1-<>)
      val tr02 = Transition(() ⇒ (1, 2))
      val tr1 = Transition((x: Int) ⇒ x + 1)
      val tr12 = Transition((x: Int) ⇒ (1, 2))
      val tr2 = Transition((x: Int, y: Int) ⇒ x + 1)
      val tr3 = Transition((x: Int, y: Int, z: Int) ⇒ (x + y + z + "")-<>)
      val tr32 = Transition((x: Int, y: Int, z: Int) ⇒ (x + y + z + "", 10))

      // Given a Marking

      // And two Transition Functions
      val tr33 = Transition((x: Int, y: Int, z: Int) ⇒ (x + y + z + "")-<>)
      val tr11 = Transition((x: Int) ⇒ (x + 1000)-<>)

      // Builder that allows building a Net structure and returns Seq of Arc
      val topology =
        buildPetriNet(
          //          (p1, p2, p3) ~> tr33 ~>> |>(p5),
          |>(p1) ~> tr11 ~>> |>(p2)
        )

      val props = Props(new PetriNetInstance[Place, RuntimeTransition, Unit](
        topology,
        instanceSettings,
        petriNetRuntime.asInstanceOf[PetriNetRuntime[Place, RuntimeTransition, Unit, Any]],
        placeId,
        transitionId)
      )

      val instance = system.actorOf(props)

      val initialMarking = Marking(p1(1))

      import scala.concurrent.duration._

      implicit val timeout = Timeout(360 seconds)

      val result = instance.ask(Initialize(initialMarking))
      val msg = Await.result(result, timeout.duration)

      println(msg)
    }
  }
}
