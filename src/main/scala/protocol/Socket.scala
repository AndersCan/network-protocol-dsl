package protocol

import scala.collection.mutable.ArrayBuffer

/**
 * Created by aoc4 on 05/03/15.
 */
class Socket {

  val states: scala.collection.mutable.ArrayBuffer[Validator] = ArrayBuffer()

  def send(socket: Socket, v: Validator) = {
    this addState v
    // add await to receiver, socket
    socket.await(socket, v)
  }

  def await(socket: Socket, v: Validator) = {
    socket addState v
  }


  // todo - send or receive
  def addState(v: Validator) = {
    states.append(v)
  }

  def compile = {
    //    states.toList
    new Protocol(states)
  }

}

class Protocol(states: ArrayBuffer[Validator]) {
  def validateMessage(input: String) = {
    val expected = states.head
    expected.f(input)
  }
}
