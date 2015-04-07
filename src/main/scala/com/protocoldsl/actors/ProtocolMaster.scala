package com.protocoldsl.actors

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import akka.pattern.ask
import akka.util.{Timeout, ByteString}
import com.protocoldsl.protocol.Protocol

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Created by anders on 04/03/15.
 */

object ProtocolMaster {
  def props(protocol: Protocol, connection: ActorRef, child: ActorRef) = Props(classOf[ProtocolMaster], protocol, connection, child)
}

case class ToChildMessage(data: ByteString)

case class SendToConnection(data: ByteString)

// use to forget last received message and get new message
//case class ForgetLast()

// use to forget last received message send message to client and get new message
//case class ForgetLastWithMessage(data: ByteString)


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
    case PeerClosed => context stop self
    case _ => println("Unknown message sent to ProtocolMaster")
  }

  def commitSuicide() = {
    // TODO - Send error to client?
    // TODO - Allow user to set suicide rules
    try {
      implicit val timeout = Timeout(1.seconds)
      connection ! PoisonPill
      val stopped2: Future[Any] = child ? PoisonPill
      Await.result(stopped2, 5.seconds)
      // the actor has been stopped
      self ! PoisonPill
    } catch {
      // the actor wasn't stopped within 5 seconds
      case e: akka.pattern.AskTimeoutException =>
    }
  }
}
