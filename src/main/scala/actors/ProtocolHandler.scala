package actors

import java.net.InetSocketAddress

import akka.actor.{Props, Actor}
import akka.io.Tcp
import akka.io.Tcp.{PeerClosed, Write, Received}
import protocol.Protocol

/**
 * Created by anders on 04/03/15.
 */

object ProtocolHandler {
  def props(protocol: Protocol) = Props(classOf[Server], protocol)
}

class ProtocolHandler(protocol: Protocol) extends Actor {

  import Tcp._

  def receive = {
    case Received(data) => {
      // ensure protocol is followed
      // data is of type expected
      if (protocol.consume(data.utf8String)) {
        sender() ! Write(data)
      } else {
        // break
        println("bad value - protocol not followed")
      }
    }
    case PeerClosed => context stop self
  }
}
