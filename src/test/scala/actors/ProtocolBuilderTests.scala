package actors

import akka.actor.ActorSystem
import akka.io.Tcp.Received
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.ByteString
import com.protocoldsl.actors.{ProtocolMaster, ToChildMessage}
import com.protocoldsl.protocol.{ProtocolBuilder, Validator}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._

/**
 * Created by aoc4 on 02/04/15.
 */
class ProtocolBuilderTests(_system: ActorSystem) extends TestKit(_system) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {
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
    "not send invalid message" in {

      val child = TestProbe()
      val connection = TestProbe()
      val proto = (new ProtocolBuilder() send isNothing).compile
      val handler = system.actorOf(ProtocolMaster.props(proto, connection.ref, child.ref))

      handler ! Received(simpleMessage)
//      child expectMsg(5000.millis, AnyRef)
//      connection expectMsg(5000.millis, PoisonPill)

    }

  }
}

