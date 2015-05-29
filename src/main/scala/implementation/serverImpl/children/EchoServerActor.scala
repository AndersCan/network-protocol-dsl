package implementation.serverImpl.children

/**
 * Created by aoc4 on 28/05/15.
 */

import akka.actor.{Actor, Props}
import akka.util.ByteString
import com.protocoldsl.actors.{Initiation, ChildFinished, SendToConnection, ToChildMessage}

/**
 * Created by anders on 04/03/15.
 */

object EchoServerActor {
  def props() = Props(classOf[EchoServerActor])
}

class EchoServerActor extends Actor {

  def receive = {
    case ToChildMessage(data) =>
      sender() ! SendToConnection(ByteString.fromString(data + "\n"))
    case Initiation =>
      println("Starting...")
    case err@_ =>
      println("What..." + err)
      sender() ! ChildFinished
  }
}