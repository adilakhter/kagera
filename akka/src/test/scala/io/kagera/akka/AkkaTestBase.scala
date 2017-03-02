package io.kagera.akka

import java.util.UUID

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import akka.cluster.sharding.ShardRegion.Passivate
import akka.testkit.{ ImplicitSender, TestKit }
import com.typesafe.config.ConfigFactory
import io.kagera.akka.actor.PetriNetInstance
import io.kagera.api.colored.ExecutablePetriNet
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
      |
      |}
      |
      |kagera {
      |  encryption {
      |    secret = "0123456789012345"
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
}

abstract class AkkaTestBase extends TestKit(ActorSystem("testSystem", AkkaTestBase.defaultTestConfig))
    with WordSpecLike
    with ImplicitSender
    with BeforeAndAfterAll {

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

  case object GetChild
  class MockShardActor(childActorProps: Props, childActorName: String = UUID.randomUUID().toString) extends Actor {
    val childActor = context.actorOf(childActorProps, childActorName)

    def receive = {
      case GetChild       ⇒ sender() ! childActor
      case Passivate(msg) ⇒ childActor ! msg
      case msg @ _        ⇒ childActor forward msg
    }
  }

  def createPetriNetActor[S](props: Props, name: String)(implicit system: ActorSystem): ActorRef = {
    val mockShardActorProps = Props(new MockShardActor(props, name))
    system.actorOf(mockShardActorProps)
  }

  def createPetriNetActor[S](petriNet: ExecutablePetriNet[S], processId: String = UUID.randomUUID().toString)(implicit system: ActorSystem): ActorRef = {
    createPetriNetActor(PetriNetInstance.props(petriNet), processId)
  }

}
