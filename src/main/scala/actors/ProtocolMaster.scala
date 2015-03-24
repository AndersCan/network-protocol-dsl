package actors

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import akka.util.ByteString
import protocol.Protocol

/**
 * Created by anders on 04/03/15.
 */

object ProtocolMaster {
  def props(protocol: Protocol, connection: ActorRef, child: ActorRef) = Props(classOf[ProtocolMaster], protocol, connection, child)
}

case class ToChildMessage(data: ByteString)

case class ToProtocolMaster(data: ByteString)

// use to forget last received message and get new message
case class ForgetLast()

// use to forget last received message send message to client and get new message
case class ForgetLastWithMessage(data: ByteString)


//case class ChildState(msg: ByteString)
/**
 * ProtocolMaster Handles the incoming messages sent from a user and checks whether it obeys the defined protocol
 * @param protocol protocol that is to be followed
 * @param connection TCP Actor connection
 * @param child Actor that uses the received messages
 */
class ProtocolMaster(protocol: Protocol, connection: ActorRef, child: ActorRef) extends Actor {


  import akka.io.Tcp._


  def receive = {

    case ToProtocolMaster(data) =>
      println("PARENT: Got Message")
      //println(data.utf8String)
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
      val msg = protocol.validateReceivedMessage(data.utf8String)
      if (msg.isRight) {
        //        sender() ! Write(data)
        child ! ToChildMessage(data)
      } else {
        println(msg.left)
        // Close connection
        // todo send error to client?
        // todo send poison pill to child?
        self ! PoisonPill
      }
    }
    case PeerClosed => context stop self
    case _ => println("Unknown message sent to ProtocolMaster")
  }
}
