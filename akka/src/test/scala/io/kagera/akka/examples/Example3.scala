package io.kagera.akka.examples

import akka.actor.Props
import akka.util.Timeout
import io.kagera.akka.AkkaTestBase
import io.kagera.akka.actor.PetriNetInstance
import io.kagera.akka.actor.PetriNetInstanceProtocol.{ FireTransition, Initialize, Initialized }
import io.kagera.api.{ Marking, PetriNet, _ }
import io.kagera.dsl.experiment.{ Place, RuntimeTransition, Transition, buildPetriNet, petriNetRuntime, placeId, transitionId, |>, _ }
import org.scalatest.Matchers._

import scala.concurrent.ExecutionContext

object Example3 extends App {
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

  val doGroceries = Transition(() ⇒ (Tomatoes(""), Pasta("")))
  val boilThePasta = Transition((x: Pasta) ⇒ BoiledPasta("")-<>)
  val cookTomatoSauce = Transition((x: Tomatoes) ⇒ TomatoSauce("")-<>)
  val serveDinner = Transition((x: BoiledPasta, y: TomatoSauce) ⇒ { println("processing done: "); Done()-<> })

  val net =
    buildPetriNet(
      |>(doGroceries) ~>> (pTomatoes, pPasta),
      |>(pPasta) ~> boilThePasta ~>> |>(pBoiledPasta),
      |>(pTomatoes) ~> cookTomatoSauce ~>> |>(pTomatoSauce),
      (pBoiledPasta, pTomatoSauce) ~> serveDinner ~>> |>(pDone)
    )

  val initialMarking = Marking.empty[Place]

  val instance = system.actorOf(petriNetProps(net))

  instance ! Initialize(initialMarking)
  expectMsgClass(classOf[Initialized])

  instance ! FireTransition(doGroceries.id, (Tomatoes(""), Pasta("")))

}
