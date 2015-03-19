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

class Socket {

  val states: scala.collection.mutable.ArrayBuffer[MessageType] = ArrayBuffer()

  def send(socket: Socket, v: Validator) = {
    this addState Receive(v)
    // add await to receiver, socket
    socket.await(socket, v)
  }

  def await(socket: Socket, v: Validator) = {
    socket addState Send(v)
  }

  // todo - send or receive
  def addState(m: MessageType) = {
    states.append(m)
  }

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

  // todo - Duplication of code in methods and 'var' usage
  def validateSendMessage(input: String) = {
    var msgType = states(stateIndex)
    stateIndex += 1
    msgType match {
      case loop: LoopToStep =>
        stateIndex = loop.i
        msgType = states(stateIndex)
      case _ =>
    }
    msgType match {
      case Receive(v) =>
        // Protocol is in receive state, not send
        println("err. not in receive")
      case _ => println("OK!")
    }
    msgType.v.f(input)
  }

  def validateReceivedMessage(input: String) = {
    var msgType = states(stateIndex)
    stateIndex += 1
    msgType match {
      case loop: LoopToStep =>
        stateIndex = loop.i
        msgType = states(stateIndex)
      case _ =>
    }
    msgType match {
      case Send(v) =>
        // Protocol is in send state, not receive
        println("err. not in send")
      case _ => println("OK!")
    }
    msgType.v.f(input)
  }

}
