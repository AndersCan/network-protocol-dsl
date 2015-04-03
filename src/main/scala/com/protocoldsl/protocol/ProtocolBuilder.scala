package com.protocoldsl.protocol

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
  def apply(states: List[MessageType]): ProtocolBuilder = {
    new ProtocolBuilder(states)
  }

  def loop(builder: ProtocolBuilder): ProtocolBuilder = {
    // builder is first step of loop.
    // case nil = go back to first step
    builder.addState(Loop(builder))
  }
}

class ProtocolBuilder(val states: List[MessageType]) {

  //  val states: scala.collection.mutable.ArrayBuffer[MessageType] = ArrayBuffer()

  def this() {
    this(List())
  }

  /**
   * Appends a new Send request with the given validator, v.
   * @param v validates expected message
   * @return new ProtocolBuilder with Send(v) appended.
   */
  def send(v: Validator): ProtocolBuilder = {
    this addState Send(v)
  }

  /**
   * Appends a new Receive request with the given validator, v.
   * @param v validates expected message
   * @return new ProtocolBuilder with Receive(v) appended.
   */
  def receive(v: Validator): ProtocolBuilder = {
    this addState Receive(v)
  }

  /**
   * Allows the protocol to branch, either left or right based on the given input.
   * @param left ProtocolBuilder to follow given a Left result
   * @param v validates message and picks goes either Left or Right
   * @param right ProtocolBuilder to follow given a Right result
   * @return new ProtocolBuilder with Branch(left,v,right) appended.
   */
  def branch(left: ProtocolBuilder, v: Validator, right: ProtocolBuilder): ProtocolBuilder = {
    this addState Branch(v, left, right)
  }

  private def addState(m: MessageType): ProtocolBuilder = {
    ProtocolBuilder(states.::(m))
  }

  /**
   * Returns the resulting Protocol from the ProtocolBuilder.
   * @return Protocol
   */
  def compile = {
    new Protocol(states)
  }

}

class Protocol(private var protocolStates: List[MessageType]) {
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
    protocolStates.head match {
      case branch: Branch =>
        val branchPath = branch.v.f(input)
        if (branchPath.isRight) {
          // going to the right
          println("Going Right")
          protocolStates = branch.right.compile.protocolStates
        } else {
          println("Going Left")
          protocolStates = branch.left.compile.protocolStates
        }
        getMessageType(input)
      case loop: Loop =>
        protocolStates = loop.pb.compile.protocolStates
        getMessageType(input)
      case msg: MessageType =>
        protocolStates = protocolStates.tail
        msg
      case _ =>
        sys.error("unknown message type")
    }
  }
}
