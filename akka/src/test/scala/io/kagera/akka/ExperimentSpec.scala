package io.kagera.akka

import akka.actor.Props
import akka.util.Timeout
import io.kagera.akka.actor.PetriNetInstance
import io.kagera.akka.actor.PetriNetInstanceProtocol._
import io.kagera.api.{ Marking, PetriNet }
import io.kagera.dsl.experiment._
import io.kagera.execution.PetriNetRuntime
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar

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

      val p1 = Place[Int](id = "t1")
      val p2 = Place[Int](id = "t2")
      val p3 = Place[Int](id = "t3")

      val t1 = Transition((x: Int) ⇒ (x + 100)-<>)
      val t2 = Transition((x: Int) ⇒ (x * 2)-<>)

      // Builder that allows building a Net structure and returns Seq of Arc
      val sequence =
        buildPetriNet(
          |>(p1) ~> t1 ~>> |>(p2),
          |>(p2) ~> t2 ~>> |>(p3)
        )

      val instance = system.actorOf(petriNetProps(sequence))
      val initialMarking = Marking(p1(1))

      import scala.concurrent.duration._

      implicit val timeout = Timeout(360 seconds)

      val result = instance ! Initialize(initialMarking)

      expectMsgClass(classOf[Initialized])
      //      expectMsgPF { case TransitionFired(_, ) =>}

      //      println(msg)
    }
  }
}
