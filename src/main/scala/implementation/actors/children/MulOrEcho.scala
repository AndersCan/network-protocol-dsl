package implementation.actors.children

import akka.actor.{Actor, Props}
import akka.util.ByteString
import com.protocoldsl.actors.{ChildFinished, SendToConnection, ToChildMessage}

/**
 * Created by anders on 04/03/15.
 */

object MulOrEcho {
  def props() = Props(classOf[MulOrEcho])
}

case class IsInt(n: Int)

class MulOrEcho extends Actor {

  def receive = {
    case ToChildMessage(data) =>
      data match {
        case IsInt(n) =>
          context become mulServer
          self ! ToChildMessage(data)
        case _ =>
          context become echoServer
          self.tell(ToChildMessage(data), sender())
      }
  }

  var isFirstNum = true
  var firstNum = 0

  def mulServer: Receive = {
    case ToChildMessage(data) =>
      data match {
        case IsInt(n) if isFirstNum =>
          firstNum = n;
        case IsInt(n) =>
          sender() ! SendToConnection(ByteString.fromString(s"${firstNum * n}\r\n"))
      }
      isFirstNum = !isFirstNum
    case a =>
      sender() ! SendToConnection(ByteString.fromString(s"$a\r\n"))
      sender() ! ChildFinished
  }

  def echoServer: Receive = {
    case ToChildMessage(data) =>
      sender() ! SendToConnection(ByteString.fromString(data + "\n"))
    case _ => sender() ! ChildFinished
  }
}