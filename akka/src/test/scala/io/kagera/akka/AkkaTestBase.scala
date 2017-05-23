package io.kagera.akka

import java.util.UUID

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import akka.cluster.sharding.ShardRegion.Passivate
import akka.testkit.{ ImplicitSender, TestKit }
import com.typesafe.config.ConfigFactory
import fs2.Strategy
import io.kagera.akka.AkkaTestBase.MockShardActor
import io.kagera.akka.actor.PetriNetInstance.Settings
import io.kagera.akka.actor.{ AkkaObjectSerializer, PetriNetInstance }
import io.kagera.api.colored
import io.kagera.api.colored.{ ColoredPetriNet, ColoredTokenGame, ColoredTransitionTaskProvider, Place, Transition }
import io.kagera.execution.{ Instance, JobExecutor, JobPicker }
import io.kagera.persistence.Encryption.NoEncryption
import org.scalatest.{ BeforeAndAfterAll, WordSpecLike }

object AkkaTestBase {

  val defaultTestConfig = ConfigFactory.parseString(
    """
      |
      |akka {
      |  loggers = ["akka.testkit.TestEventListener"]
      |  test.timefactor = 4
      |  persistence {
      |    journal.plugin = "inmemory-journal"
      |    snapshot-store.plugin = "inmemory-snapshot-store"
      |  }
      |}
      |
      |inmemory-read-journal {
      |  write-plugin = "inmemory-journal"
      |  offset-mode = "sequence"
      |  ask-timeout = "10s"
      |  refresh-interval = "50ms"
      |  max-buffer-size = "100"
      |}
      |
      |logging.root.level = WARN
    """.stripMargin)

  case object GetChild
  class MockShardActor(childActorProps: Props, childActorName: String = UUID.randomUUID().toString) extends Actor {
    val childActor = context.actorOf(childActorProps, childActorName)

    def receive = {
      case GetChild       ⇒ sender() ! childActor
      case Passivate(msg) ⇒ childActor ! msg
      case msg @ _        ⇒ childActor forward msg
    }
  }
}

abstract class AkkaTestBase extends TestKit(ActorSystem("testSystem", AkkaTestBase.defaultTestConfig))
    with WordSpecLike
    with ImplicitSender
    with BeforeAndAfterAll {

  private val coloredJobPicker = new JobPicker[Place, Transition](new ColoredTokenGame()) {
    override def isFireable[S](instance: Instance[Place, Transition, S], t: Transition[_, _, _]): Boolean =
      t.isAutomated && !instance.isBlockedReason(t).isDefined
  }

  def coloredProps[S](topology: ColoredPetriNet, settings: Settings): Props =
    Props(new PetriNetInstance[Place, Transition, S](
      topology,
      settings,
      coloredJobPicker,
      new JobExecutor[S, Place, Transition](topology, new ColoredTransitionTaskProvider[S], t ⇒ t.exceptionStrategy)(settings.evaluationStrategy),
      t ⇒ t.updateState.asInstanceOf[(S ⇒ Any ⇒ S)],
      colored.placeIdentifier,
      colored.transitionIdentifier)
    )

  override def afterAll() = {
    super.afterAll()
    shutdown(system)
  }

  def expectMsgInAnyOrderPF[Out](pfs: PartialFunction[Any, Out]*): Unit = {
    if (pfs.nonEmpty) {
      val total = pfs.reduce((a, b) ⇒ a.orElse(b))
      expectMsgPF() {
        case msg @ _ if total.isDefinedAt(msg) ⇒
          val index = pfs.indexWhere(pf ⇒ pf.isDefinedAt(msg))
          val pfn = pfs(index)
          pfn(msg)
          expectMsgInAnyOrderPF[Out](pfs.take(index) ++ pfs.drop(index + 1): _*)
      }
    }
  }

  val instanceSettings = Settings(
    evaluationStrategy = Strategy.fromCachedDaemonPool("Kagera.CachedThreadPool"),
    idleTTL = None,
    serializer = new AkkaObjectSerializer(system, NoEncryption)
  )

  def createPetriNetActor[S](props: Props, name: String)(implicit system: ActorSystem): ActorRef = {
    val mockShardActorProps = Props(new MockShardActor(props, name))
    system.actorOf(mockShardActorProps)
  }

  def createPetriNetActor[S](petriNet: ColoredPetriNet, processId: String = UUID.randomUUID().toString)(implicit system: ActorSystem): ActorRef = {
    createPetriNetActor(coloredProps(petriNet, instanceSettings), processId)
  }
}
