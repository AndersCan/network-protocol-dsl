package actors

import akka.actor.ActorSystem
import akka.io.Tcp.{Write, Received}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.ByteString
import com.protocoldsl.actors.{SendToConnection, ProtocolMaster, ToChildMessage}
import com.protocoldsl.protocol.{ProtocolBuilder, Validator}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._

/**
 * Created by aoc4 on 02/04/15.
 */
class ProtocolMasterTests(_system: ActorSystem) extends TestKit(_system) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {
  def this() = this(ActorSystem("ProtocolBuilderTests"))

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
  }


  val isAnything = new Validator(_ => Right(true))
  val isNothing = new Validator(_ => Left("isNothing will always give a Left result"))
  val simpleMessage = ByteString.fromString("Simple message")

  "The ProtocolMaster actor" must {
    "send a valid message to its child" in {
      val child = TestProbe()
      val connection = TestProbe()
      val proto = (new ProtocolBuilder() receive isAnything).compile
      val handler = system.actorOf(ProtocolMaster.props(proto, connection.ref, child.ref))

      handler ! Received(simpleMessage)
      child expectMsg(5000.millis, ToChildMessage(simpleMessage))
    }
  }
  it must {
    "not send invalid message to its child" in {
      val child = TestProbe()
      val connection = TestProbe()
      val proto = (new ProtocolBuilder() send isNothing).compile
      val handler = system.actorOf(ProtocolMaster.props(proto, connection.ref, child.ref))

      handler ! Received(simpleMessage)
      child expectNoMsg 2000.millis
    }
  }
  it must {
    "forward a valid message from child to connection" in {
      val child = TestProbe()
      val connection = TestProbe()
      val proto = (new ProtocolBuilder() send isAnything).compile
      val handler = system.actorOf(ProtocolMaster.props(proto, connection.ref, child.ref))

      handler ! SendToConnection(simpleMessage)
      connection expectMsg(1000.millis, Write(simpleMessage))
    }
  }
  it must {
    "must not forward a invalid message from child to connection" in {
      val child = TestProbe()
      val connection = TestProbe()
      val proto = (new ProtocolBuilder() receive isNothing).compile
      val handler = system.actorOf(ProtocolMaster.props(proto, connection.ref, child.ref))

      handler ! SendToConnection(simpleMessage)
      connection expectNoMsg 1000.millis
    }
  }
}

