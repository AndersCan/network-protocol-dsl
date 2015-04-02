package com.protocoldsl.protocol

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

case class Loop(pb: ProtocolBuilder, v: Validator = new Validator(_ => Left("validate run on Loop step"))) extends MessageType()

case class Branch(v: Validator, left: ProtocolBuilder, right: ProtocolBuilder) extends MessageType()

object ProtocolBuilder {
  def loop(builder: ProtocolBuilder): ProtocolBuilder = {
    // builder is first step of loop.
    // case nil = go back to first step
    builder.addState(Loop(builder))
    builder
  }
}

class ProtocolBuilder(val states: scala.collection.mutable.ArrayBuffer[MessageType]) {

  //  val states: scala.collection.mutable.ArrayBuffer[MessageType] = ArrayBuffer()

  def this() {
    this(ArrayBuffer())
  }

  def send(socket: ProtocolBuilder, v: Validator) = {
    this addState Send(v)
    // add await to receiver, socket
    socket.receive(socket, v)
  }

  def send(v: Validator): ProtocolBuilder = {
    this addState Send(v)
    this
  }

  def receive(socket: ProtocolBuilder, v: Validator) = {
    socket addState Receive(v)
  }

  def receive(v: Validator): ProtocolBuilder = {
    this addState Receive(v)
    this
  }

  def branch(left: ProtocolBuilder, v: Validator, right: ProtocolBuilder): ProtocolBuilder = {
    states.append(Branch(v, left, right))
    this
  }

  def addState(m: MessageType) = {
    states.append(m)
  }


  def compile = {
    new Protocol(states)
  }

}

class Protocol(var states: ArrayBuffer[MessageType]) {
  // Index of where in 'states' list we are
  //  var stateIndex = 0
  // TODO - Combine send & receive methods?
  def validateSendMessage(input: String) = {
    val msgType: MessageType = getMessageType(input)

    msgType match {
      case Receive(v) =>
        Left("protocol violated - sending when should be receiving")
      case _ => msgType.v.f(input)
    }
  }

  def validateReceivedMessage(input: String) = {
    val msgType: MessageType = getMessageType(input)

    msgType match {
      case Send(v) =>
        Left("protocol violated - receiving when should be sending")
      case _ => msgType.v.f(input)
    }
  }

  // Branch and Looping cases are handled here
  private def getMessageType(input: String): MessageType = {
    println(states.head)
    states.head match {
      case branch: Branch =>
        val branchPath = branch.v.f(input)
        if (branchPath.isRight) {
          // going to the right
          println("Going Right")
          states = branch.right.compile.states
        } else {
          println("Going Left")
          states = branch.left.compile.states
        }
        getMessageType(input)
      case loop: Loop =>
        states = loop.pb.compile.states
        getMessageType(input)
      case msg: MessageType =>
        states = states.tail
        msg
      case _ =>
        sys.error("unknown message type")
    }
  }
}
