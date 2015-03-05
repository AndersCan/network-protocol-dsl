package actors

import akka.actor.{PoisonPill, Actor, Props}
import protocol.Protocol

/**
 * Created by anders on 04/03/15.
 */

object ProtocolHandler {
  def props(protocol: Protocol) = Props(classOf[ProtocolHandler], protocol)
}

class ProtocolHandler(protocol: Protocol) extends Actor {

  import akka.io.Tcp._

  def receive = {
    case Received(data) => {
      // ensure protocol is followed
      // data is of type expected
      val msg = protocol.validateMessage(data.utf8String)
//      val msg = protocol.validateMessage(data.mkString)

      if (msg.isRight) {
        sender() ! Write(data)
      } else {
        println(msg.left)
        // Close connection
        self ! PoisonPill
      }
    }
    case PeerClosed => context stop self
  }
}
