package com.protocoldsl.actors

import akka.actor._
import akka.util.ByteString
import com.protocoldsl.protocol.{END, Protocol}

/**
 * Created by anders on 04/03/15.
 */

object ProtocolMonitor {
  def props(protocol: Protocol, connection: ActorRef, child: ActorRef) = Props(classOf[ProtocolMonitor], protocol, connection, child)
}

case class ToChildMessage(data: ByteString)

case class SendToConnection(data: ByteString)


// use to forget last received message and get new message
//case class ForgetLast()

// use to forget last received message send message to client and get new message
//case class ForgetLastWithMessage(data: ByteString)

// ProtocolMonitor

//case class ChildState(msg: ByteString)
/**
 * ProtocolMaster Handles the incoming messages sent from a user and checks whether it obeys the defined protocol
 * @param protocol protocol that is to be followed
 * @param connection TCP Actor connection
 * @param child Actor that uses the received messages
 */
class ProtocolMonitor(protocol: Protocol, connection: ActorRef, child: ActorRef) extends Actor {


  import akka.io.Tcp._


  def receive = {

    case SendToConnection(data) =>
      val msg = protocol.validateSendMessage(data.utf8String)
      if (msg.isRight) {
        connection ! Write(data)
      } else {
        println(msg.left)
        // Close connection
        commitSuicide()
      }

    case Received(data) => {
      // Todo - Stop dropping new line characters?
      // Remove \n from end of line
      val msg = protocol.validateReceivedMessage(data.utf8String.dropRight(2))
      if (msg.isRight) {
        child ! ToChildMessage(data)
      } else {
        println(msg.left)
        // Close connection
        commitSuicide()
      }
    }
    case PeerClosed =>
      commitSuicide()
    case END =>
      // End of Protocol Reached.
      commitSuicide()


    case _ => println("Unknown message sent to ProtocolMonitor")
  }


  def commitSuicide() = {
    // TODO - Send error to client?
    // TODO - Allow user to set suicide rules
    //    http://doc.akka.io/docs/akka/snapshot/scala/scheduler.html
    try {
      connection ! PoisonPill
      child ! PoisonPill
      // the actor has been stopped
      self ! PoisonPill
    } catch {
      // the actor wasn't stopped within 5 seconds
      case e: akka.pattern.AskTimeoutException =>
    }
  }
}