package actors

import akka.actor.{Props, Actor}
import akka.io.Tcp
import akka.io.Tcp.{Write, PeerClosed}
import akka.util.ByteString
import protocol.Protocol

/**
 * Created by anders on 04/03/15.
 */

object SimplisticHandler {
  def props() = Props(classOf[SimplisticHandler])
}


class SimplisticHandler extends Actor {

  //  import Tcp._
  //  var isSet = false
  var isSet = true
  var firstNum = 0

  def receive = {

    case ToChildMessage(data) =>
      println("RECEIVED")
      println(data.utf8String)

      if (isSet) {
        firstNum = data.utf8String.dropRight(2).toInt
        isSet = !isSet
      } else {
        //        self ! InternalMessage(firstNum, data.utf8String.dropRight(2).toInt)
        val y = data.utf8String.dropRight(2).toInt
        isSet = !isSet
        sender() ! ToProtocolMaster(ByteString.fromString(s"${firstNum * y}\r\n"))
        //        var x = ByteString.fromString("")
      }
  }
}