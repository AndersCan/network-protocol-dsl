package protocol

import scala.collection.mutable.ArrayBuffer

/**
 * Created by anders on 04/03/15.
 */

object Creator extends App {

  val p = Creator.generateProtocol((x: Int) => {
    "x"
  })

  println(p)

  def generateProtocol(intToString: (Int) => String) = {
    5
  }


  val c = new Socket()
  val s = new Socket()

  // todo Create function instead of String compares
  c send(s, "String")
  s send(c, "String")

  println(c.states)
  println(s.states)


}

class Socket {
  def send(socket: Socket, clazz: String) = {
    // add await to s
    socket.await(socket, clazz)
    this addState("!", clazz)
  }

  def await(socket: Socket, clazz: String) = {
    socket addState("?", clazz)
  }

  val states: scala.collection.mutable.ArrayBuffer[String] = ArrayBuffer()

  def addState(kind: String, clazz: String) = {
    states.append(kind + clazz)
    //    states ++ kind ++ clazz
  }

  def compile = {
    //    states.toList
    new Protocol(states)
  }

}

class Protocol(states: ArrayBuffer[String]) {
  def consume(actual: String) = {
    val expected = states.head
    states.take(1)
    expected == actual.getClass.getSimpleName
  }
}