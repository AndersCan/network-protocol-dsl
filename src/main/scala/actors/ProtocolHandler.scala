package actors

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import akka.util.ByteString
import protocol.Protocol

/**
 * Created by anders on 04/03/15.
 */

object ProtocolHandler {
  def props(protocol: Protocol, connection: ActorRef, child: ActorRef) = Props(classOf[ProtocolHandler], protocol, connection, child)
}

case class ChildMessage(data: ByteString)

case class ParentMessage(data: ByteString)

// use to forget last received message and get new message
case class ForgetLast()

// use to forget last received message send message to client and get new message
case class ForgetLastWithMessage(data: ByteString)


//case class ChildState(msg: ByteString)

class ProtocolHandler(protocol: Protocol, connection: ActorRef, child: ActorRef) extends Actor {


  import akka.io.Tcp._


  def receive = {

    case ParentMessage(data) =>
      println("PARENT: Got Message")
      println(data.utf8String)
      //      sender() ! Write(data)
      val msg = protocol.validateSendMessage(data.utf8String)
      if (msg.isRight) {
        connection ! Write(data)
      } else {
        println(msg.left)
        // Close connection
        self ! PoisonPill
      }
    case Received(data) => {
      // ensure protocol is followed
      // data is of type expected
      val msg = protocol.validateReceivedMessage(data.utf8String)
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
