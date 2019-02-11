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
import io.kagera.akka.examples.Example2.analysis
import io.kagera.api._
import io.kagera.dsl.experiment._
import org.scalatest.Matchers._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar

import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration._

object Example2 extends App {
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

  case class Pasta(id: String)
  case class BoiledPasta(id: String)
  case class Tomatoes(id: String)
  case class TomatoSauce(id: String)
  case class Done()

  case class StartCooking()

  val pInit = Place[StartCooking](id = "pStart")
  val pPasta = Place[Pasta](id = "pPasta")
  val pBoiledPasta = Place[BoiledPasta](id = "pboiledPasta")

  val pTomatoes = Place[Tomatoes](id = "pTomatoes")
  val pTomatoSauce = Place[TomatoSauce](id = "pTomatoSauce")
  val pDone = Place[Done](id = "pDone")

  val doGroceries = Transition((x: StartCooking) ⇒ (Tomatoes(""), Pasta("")))
  val boilThePasta = Transition((x: Pasta) ⇒ BoiledPasta("")-<>)
  val cookTomatoSauce = Transition((x: Tomatoes) ⇒ TomatoSauce("")-<>)
  val serveDinner = Transition((x: BoiledPasta, y: TomatoSauce) ⇒ { println("processing done: "); Done()-<> })

  val net =
    buildPetriNet(
      |>(pInit) ~> (doGroceries) ~>> (pTomatoes, pPasta),
      |>(pPasta) ~> boilThePasta ~>> |>(pBoiledPasta),
      |>(pTomatoes) ~> cookTomatoSauce ~>> |>(pTomatoSauce),
      (pBoiledPasta, pTomatoSauce) ~> serveDinner ~>> |>(pDone)
    )

  val initialMarking = Marking((pInit(StartCooking())))
  val initialMarking2 = Marking.empty[Place]

  val analysis = new PetriNetAnalysis(net)
  analysis.isReachable(initialMarking.multiplicities, Map(pDone -> 1)) shouldBe (true)

  val res = analysis.isReachable(initialMarking2.multiplicities, Map(pDone -> 1))

  println("isreachable: " + res)

  val instance = system.actorOf(petriNetProps(net))

  instance ! Initialize(initialMarking)
  expectMsgClass(classOf[Initialized])

  instance ! FireTransition(doGroceries.id, (Tomatoes(""), Pasta("")))

}
