package com.protocoldsl.actors

import akka.actor._
import akka.util.ByteString
import com.protocoldsl.protocol.{Validator, Protocol}

/**
 * Created by anders on 04/03/15.
 */

object ProtocolMonitor {
  def props(protocol: Protocol, connection: ActorRef, consumer: ActorRef) = Props(classOf[ProtocolMonitor], protocol, connection, consumer)
}

case class ToChildMessage(data: Any)

case class ToConnection(data: String)

case class ProtocolEnded(reason: Any)

case object ChildFinished

case object Initiation

case class DelayedValidation(input: String, validator: Validator)

/**
 * ProtocolMaster Handles the incoming messages sent from a user and checks whether it obeys the defined protocol
 * @param protocol protocol that is to be followed
 * @param connection TCP Actor connection
 * @param consumer Actor that uses the received messages
 */
class ProtocolMonitor(protocol: Protocol, connection: ActorRef, consumer: ActorRef) extends Actor {


  import akka.io.Tcp._


  def receive = {

    case ToConnection(data) =>
      val msg = protocol.validateSendMessage(data)
      if (msg.isRight) {
        connection ! Write(ByteString.fromString(data))
      } else {
        //Protocol error
        initiateStop(msg.left.get)
      }

    case Received(data) =>
      // Todo - Stop dropping new line characters?
      // Remove \n from end of line
      val msg = protocol.validateReceivedMessage(data.utf8String)
      if (msg.isRight) {
        consumer ! msg.right.get
      } else {
        // Protocol error
        initiateStop(msg.left.get)
      }
    case DelayedValidation(input, validator) =>
      val msg = validator.f(input)
      if (msg.isRight) {
        consumer ! msg.right.get
      } else {
        // Protocol error
        initiateStop(msg.left.get)
      }
    case Initiation =>
      consumer ! Initiation
    case PeerClosed =>
      initiateStop(PeerClosed)
    case ChildFinished =>
      connection ! Close
      context become waitingForShutdown
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
    case ToConnection(data) =>
      // Allow Error message to be sent to client
      connection ! Write(ByteString.fromString(data))
    case ChildFinished =>
      self ! PoisonPill
    case PeerClosed =>
      self ! PoisonPill
      println("CLOSED PM")
    case Closed => // Closed
    case err@_ => println(s"Ignoring received msg: $err")
  }

  override def preStart() = {
    self ! Initiation
  }
}
