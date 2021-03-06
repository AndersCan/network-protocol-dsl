package implementation.serverImpl.children

import akka.actor.{Actor, Props}
import com.protocoldsl.actors.{ChildFinished, ToConnection, ToChildMessage}

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
          sender() ! ToConnection(s"${firstNum * n}\r\n")
      }
      isFirstNum = !isFirstNum
    case a =>
      sender() ! ToConnection(s"$a\r\n")
      sender() ! ChildFinished
  }

  def echoServer: Receive = {
    case ToChildMessage(data) =>
      sender() ! ToConnection(data + "\n")
    case _ => sender() ! ChildFinished
  }
}