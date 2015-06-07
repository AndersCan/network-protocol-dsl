package implementation.serverImpl.performance

/**
 * Created by aoc4 on 28/05/15.
 */

import akka.actor.{Actor, Props}
import com.protocoldsl.actors.{ChildFinished, Initiation, ToConnection}
import com.protocoldsl.protocol.ValidationError

case class EchoMessage(text: String)

object EchoServerActor {
  def props() = Props(classOf[EchoServerActor])
}

class EchoServerActor extends Actor {

  def receive = {

    case EchoMessage(data) =>
      sender() ! ToConnection(data)
    case Initiation =>
      println("Starting...")
    case ValidationError(msg, exception) =>
      println(s"Error: $msg. Exception: $exception")
    case err@_ =>
      println("What..." + err)
      sender() ! ChildFinished
  }
}