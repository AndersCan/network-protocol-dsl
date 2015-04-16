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

case class Loop(pb: ProtocolBuilder, loops: Int = -1, next: ProtocolBuilder = ProtocolBuilder().end, v: Validator = new Validator(_ => Left("validate run on Loop step"))) extends MessageType()

case class Branch(branch: String => ProtocolBuilder, v: Validator = new Validator(_ => Left("validate ran on Branch step"))) extends MessageType()

case class END(v: Validator = new Validator(_ => Left("validate run on END step"))) extends MessageType()



object ProtocolBuilder {
  def apply(states: List[MessageType]): ProtocolBuilder = {
    new ProtocolBuilder(states)
  }

  def apply(): ProtocolBuilder = {
    new ProtocolBuilder()
  }

  /**
   * Adds two loop steps to the end of the ProtocolBuilder. The second loop is to ensure the first loop step is always present.
   * @param builder the ProtocolBuilder currently being created
   * @return ProtocolBuilder with a loop step that contains itself
   */
  def loop(builder: ProtocolBuilder): ProtocolBuilder = {
    builder addState Loop(builder)
  }

  def loop(builder: ProtocolBuilder, loops: Int): ProtocolBuilder = {
    builder addState Loop(builder, loops)
  }
}

class ProtocolBuilder(val states: List[MessageType]) {

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
   * Adds an infinite loop to this ProtocolBuilder
   * @return ProtocolBuilder that will loop forever
   */
  def loop(): ProtocolBuilder = {
    this addState Loop(this)
  }

  /**
   * Adds a specified amount of loops to the ProtocolBuilder.
   * @param loops how many times it should be looped
   * @return ProtocolBuilder with loops
   */
  def looped(loops: Int, after: ProtocolBuilder): ProtocolBuilder = {
    this addState Loop(this, loops, after)
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
   * Allows the protocol to branch depending on received input.
   * @param branch Branch contains a function that returns a ProtocolBuilder based on given input
   * @return new ProtocolBuilder with Branch() appended.
   */
  def branchOn(branch: MessageType): ProtocolBuilder = {
    this addState branch
  }


  // Must prepend to list.
  private def addState(m: MessageType): ProtocolBuilder = {
    ProtocolBuilder(states.:+(m))
  }

  /**
   * Returns the resulting Protocol from the ProtocolBuilder.
   * @return Protocol
   */
  def end = {
    this addState END()
    //    new Protocol((this addState END()) states)
  }

  /**
   * Returns the resulting Protocol from the ProtocolBuilder.
   * @return Protocol
   */
  def compile = {
    new Protocol(states)
  }

}

class Protocol(var protocolStates: List[MessageType]) {
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
    //    println(s"States: $protocolStates")
    protocolStates.head match {
      case branch: Branch =>
        protocolStates = branch.branch(input).compile.protocolStates
        getMessageType(input)
      case Loop(pb, 0, next, _) =>
        //        println(s"Last Loop")
        //        println(s"Loop: $protocolStates")
        protocolStates = next.compile.protocolStates
        getMessageType(input)
      case Loop(pb, -1, _, _) =>
        //        println(s"Infinite loop")
        protocolStates = pb.loop().compile.protocolStates
        getMessageType(input)
      case Loop(pb, loops, next, _) =>
        //        println(s"Current loop: $loops")
        protocolStates = pb.looped(loops - 1, next).compile.protocolStates
        getMessageType(input)
      case msg: MessageType =>
        protocolStates = protocolStates.tail
        msg
      case _ =>
        sys.error("unknown message type received in Protocol")
    }
  }

  // TODO Implement end
  //  def protocolFinished(): Boolean = {
  //    protocolStates.head match {
  //      case END => true
  //      case _ => false
  //    }
  //  }
}
