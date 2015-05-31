package implementation.serverImpl.performance

/**
 * Created by aoc4 on 28/05/15.
 */

import akka.actor.{Actor, Props}
import akka.util.ByteString
import com.protocoldsl.actors.{ChildFinished, Initiation, SendToConnection}
import com.protocoldsl.protocol.ValidationError

case class EchoMessage(text: String)

object EchoServerActor {
  def props() = Props(classOf[EchoServerActor])
}

class EchoServerActor extends Actor {

  def receive = {

    case EchoMessage(data) =>
      sender() ! SendToConnection(ByteString.fromString(data))
    case Initiation =>
      println("Starting...")
    case ValidationError(msg, exception) =>
      println(s"Error: $msg. Exception: $exception")
    case err@_ =>
      println("What..." + err)
      sender() ! ChildFinished
  }
}