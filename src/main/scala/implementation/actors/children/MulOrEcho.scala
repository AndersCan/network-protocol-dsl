package implementation.actors.children

import akka.actor.{Actor, Props}
import akka.util.ByteString
import com.protocoldsl.actors.{SendToConnection, ToChildMessage}

/**
 * Created by anders on 04/03/15.
 */

object MulOrEcho {
  def props() = Props(classOf[MulOrEcho])
}

class MulOrEcho extends Actor {

  def receive = {
    case ToChildMessage(data) =>
      if (data.utf8String.dropRight(2) forall Character.isDigit) {
        context become mulServer
        self ! ToChildMessage(data)
      } else {
        context become echoServer
        self.tell(ToChildMessage(data), sender())
      }
  }

  var firstNumber = true
  var firstNum = 0

  def mulServer: Receive = {
    case ToChildMessage(data) =>
      if (firstNumber) {
        if (!(data.utf8String.dropRight(2) forall Character.isDigit)) {
          println("I'm not expecting a String!")
        } else {
          firstNum = data.utf8String.dropRight(2).toInt
          firstNumber = !firstNumber
        }
      } else {
        val y = data.utf8String.dropRight(2).toInt
        firstNumber = !firstNumber
        sender() ! SendToConnection(ByteString.fromString(s"${firstNum * y}\r\n"))
      }
  }

  def echoServer: Receive = {
    case ToChildMessage(data) =>
      sender() ! SendToConnection(data)
  }
}