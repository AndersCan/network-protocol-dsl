package protocol

import scala.collection.mutable.ArrayBuffer

/**
 * Created by aoc4 on 05/03/15.
 */
sealed case class Validator(f: String => Either[String, Boolean])


abstract class MessageType() {
  val v: Validator
}

case class Send(v: Validator) extends MessageType()

case class Receive(v: Validator) extends MessageType()

case class LoopToStep(i: Integer, v: Validator = new Validator(_ => Left("validate run on Loop step"))) extends MessageType()

class ProtocolBuilder {

  val states: scala.collection.mutable.ArrayBuffer[MessageType] = ArrayBuffer()

  def send(socket: ProtocolBuilder, v: Validator) = {
    this addState Send(v)
    // add await to receiver, socket
    socket.receive(socket, v)
  }

  def receive(socket: ProtocolBuilder, v: Validator) = {
    socket addState Receive(v)
  }

  def addState(m: MessageType) = {
    states.append(m)
  }

  // Have to add 1 because StateIndex is incremented AFTER
  def gotoStep(i: Int): Unit = {
    states.append(LoopToStep(i))
  }

  def compile = {
    new Protocol(states)
  }

}

class Protocol(states: ArrayBuffer[MessageType]) {
  // Index of where in 'states' list we are
  var stateIndex = 0

  // todo Combine send & receive methods?
  def validateSendMessage(input: String) = {
    val msgType: MessageType = getMessageType

    msgType match {
      case Receive(v) =>
        // Protocol is in receive state, not send
        Left("protocol violated - sending when should be receiving")
      case _ => msgType.v.f(input)
    }

  }

  def validateReceivedMessage(input: String) = {
    val msgType: MessageType = getMessageType

    msgType match {
      case Send(v) =>
        // todo kill connection
        // Protocol is in send state, not receive
        Left("protocol violated - receiving when should be sending")
      case _ => msgType.v.f(input)
    }

  }

  private def getMessageType: MessageType = {
    states(stateIndex) match {
      case loop: LoopToStep =>
        stateIndex = loop.i
        getMessageType
      case msg: MessageType =>
        //        val msgType = msg
        stateIndex += 1
        msg
      case _ =>
        sys.error("unknown message type")
    }
  }
}
