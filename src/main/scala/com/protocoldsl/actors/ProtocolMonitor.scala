package com.protocoldsl.actors

import akka.actor._
import akka.util.ByteString
import com.protocoldsl.protocol.Protocol

/**
 * Created by anders on 04/03/15.
 */

object ProtocolMonitor {
  def props(protocol: Protocol, connection: ActorRef, consumer: ActorRef) = Props(classOf[ProtocolMonitor], protocol, connection, consumer)
}

case class ToChildMessage(data: Any)

case class SendToConnection(data: ByteString)

case class ProtocolEnded(reason: Any)

case object ChildFinished

case object Initiation

/**
 * ProtocolMaster Handles the incoming messages sent from a user and checks whether it obeys the defined protocol
 * @param protocol protocol that is to be followed
 * @param connection TCP Actor connection
 * @param consumer Actor that uses the received messages
 */
class ProtocolMonitor(protocol: Protocol, connection: ActorRef, consumer: ActorRef) extends Actor {


  import akka.io.Tcp._


  def receive = {

    case SendToConnection(data) =>
      val msg = protocol.validateSendMessage(data.utf8String)
      if (msg.isRight) {
        connection ! Write(data)
      } else {
        //Protocol error
        initiateStop(msg.left)
      }

    case Received(data) =>
      // Todo - Stop dropping new line characters?
      // Remove \n from end of line
      val msg = protocol.validateReceivedMessage(data.utf8String)
      if (msg.isRight) {
        consumer ! ToChildMessage(msg.right.get)
      } else {
        // Protocol error
        println("Error on: " + data.utf8String)
        initiateStop(msg.left)
      }
    case Initiation =>
      consumer ! Initiation
    case PeerClosed =>
      initiateStop(PeerClosed)
    case ChildFinished =>
      stopSelf()
    case err@ErrorClosed(msg) =>
      // Connection closed/reset by client
      initiateStop(err)
    case unknown@_ => println(s"Unknown message sent to ProtocolMonitor: $unknown")
  }

  def initiateStop(err: Any) = {
    //println(s"Initiate stop: $err")
    context become waitingForShutdown
    consumer ! ProtocolEnded(err)
  }

  def waitingForShutdown: Receive = {
    case SendToConnection(data) =>
      connection ! Write(data)
    case ChildFinished =>
      stopSelf()
    case _ => //println("Ignoring received msg")
  }

  def stopSelf() = {
    self ! PoisonPill
  }

  override def preStart() = {
    self ! Initiation
  }
}
