package protocol

import scala.collection.mutable.ArrayBuffer

/**
 * Created by aoc4 on 05/03/15.
 */
class Socket {

  def send(socket: Socket, input: Validator) = {
    // add await to s
    socket.await(socket, input)
    this addState input
  }

  def await(socket: Socket, clazz: Validator) = {
    socket addState clazz
  }

  val states: scala.collection.mutable.ArrayBuffer[Validator] = ArrayBuffer()

  // todo - send or receive
  def addState(clazz: Validator) = {
    states.append(clazz)
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
