package actors

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import akka.util.ByteString
import protocol.Protocol

/**
 * Created by anders on 04/03/15.
 */

object ProtocolHandler {
  def props(protocol: Protocol, child: ActorRef) = Props(classOf[ProtocolHandler], protocol, child)
}

case class ChildMessage(data: ByteString)

case class ParentMessage(data: ByteString)

//case class ChildState(msg: ByteString)

class ProtocolHandler(protocol: Protocol, child: ActorRef) extends Actor {


  import akka.io.Tcp._

  var connectedClient: ActorRef = null

  def receive = {

    case ParentMessage(data) =>
      println("PARENT: Got Message")
      println(data.utf8String)
      //      sender() ! Write(data)
      connectedClient ! Write(data)
    case Received(data) => {
      connectedClient = sender()
      // ensure protocol is followed
      // data is of type expected
      val msg = protocol.validateMessage(data.utf8String)
      //      val msg = protocol.validateMessage(data.mkString)
      if (msg.isRight) {
        //        sender() ! Write(data)
        child ! ChildMessage(data)
      } else {
        println(msg.left)
        // Close connection
        self ! PoisonPill
      }
    }
    case PeerClosed => context stop self
    case _ => println("Unknown message...")
  }
}
