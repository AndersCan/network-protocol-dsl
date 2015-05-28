package actors.ProtocolMonitor

import akka.actor.ActorSystem
import akka.io.Tcp.{Received, Write}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.ByteString
import com.protocoldsl.actors._
import com.protocoldsl.protocol.{ProtocolBuilder, Validator}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._

/**
 * Created by aoc4 on 02/04/15.
 */
class ProtocolMonitorTests(_system: ActorSystem) extends TestKit(_system) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {
  def this() = this(ActorSystem("ProtocolBuilderTests"))

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
  }


  val isAnything = new Validator(x => Right(x))
  val isNothing = new Validator(_ => Left("isNothing will always give a Left result"))
  val simpleMessage = ByteString.fromString("Simple message")

  "The ProtocolMaster actor" must {
    "send a valid message to its child" in {
      val child = TestProbe()
      val connection = TestProbe()
      val proto = (new ProtocolBuilder() receives isAnything).compile
      val handler = system.actorOf(ProtocolMonitor.props(proto, connection.ref, child.ref))

      handler ! Received(simpleMessage)
      child expectMsg(5000.millis, ToChildMessage("Simple message"))
    }
  }
  it must {
    "not send invalid message to its child" in {
      val child = TestProbe()
      val connection = TestProbe()
      val proto = (new ProtocolBuilder() sends isNothing).compile
      val handler = system.actorOf(ProtocolMonitor.props(proto, connection.ref, child.ref))
      handler ! Received(simpleMessage)
      child expectMsgClass(5000.millis, ProtocolEnded("").getClass)
    }
  }
  it must {
    "forward a valid message from child to connection" in {
      val child = TestProbe()
      val connection = TestProbe()
      val proto = (new ProtocolBuilder() sends isAnything).compile
      val handler = system.actorOf(ProtocolMonitor.props(proto, connection.ref, child.ref))

      handler ! SendToConnection(simpleMessage)
      connection expectMsg(1000.millis, Write(simpleMessage))
    }
  }
  it must {
    "must not forward a invalid message from child to connection" in {
      val child = TestProbe()
      val connection = TestProbe()
      val proto = (new ProtocolBuilder() receives isNothing).compile
      val handler = system.actorOf(ProtocolMonitor.props(proto, connection.ref, child.ref))

      handler ! SendToConnection(simpleMessage)
      connection expectNoMsg 1000.millis
    }
  }
}

