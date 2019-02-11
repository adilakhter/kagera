package io.kagera.akka.examples

import akka.actor.{ ActorSystem, Props }
import akka.util.Timeout
import io.kagera.akka.AkkaTestBase
import io.kagera.akka.actor.PetriNetInstance
import io.kagera.akka.actor.PetriNetInstanceProtocol.{ FireTransition, Initialize, Initialized, TransitionFired }
import io.kagera.api.{ Marking, PetriNet }
import io.kagera.dsl.experiment.{ Place, RuntimeTransition, Transition, buildPetriNet, petriNetRuntime, placeId, transitionId, |> }

import scala.concurrent.ExecutionContext
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

object Example1 extends App {
  object utils extends AkkaTestBase {
    def petriNetProps(topology: PetriNet[Place[_], RuntimeTransition[_, _]]) =
      Props(new PetriNetInstance[Place, RuntimeTransition, Unit, Unit](
        topology,
        instanceSettings,
        petriNetRuntime,
        placeId _,
        transitionId _)
      )

    import scala.concurrent.duration._
    implicit val timeout = Timeout(360 seconds)
    implicit val ec: ExecutionContext = system.dispatcher
  }

  import utils._

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

  // format: off
  // Builder that allows building a Net structure and returns Seq of Arc
  val sequence =
    buildPetriNet(
                 ~  t0 ~>       p1.-|,
      ~ p1       ~> t1 ~>       p2.-|,
      ~ p2       ~> t2 ~> (p3, p4).-|,
      ~ (p3, p4) ~> t3 ~>       p5.-|,
      ~ p5       ~> t4

    )
  // format: on

  val instance = system.actorOf(petriNetProps(sequence))
  val initialMarking = Marking.empty[Place]

  instance ! Initialize(initialMarking)
  expectMsgClass(classOf[Initialized])

  instance ! FireTransition(t0.id, 1 -<>)
  //utils.shutdown()
}
