package implementation.serverImpl.children

import akka.actor.{Actor, Props}
import com.protocoldsl.actors._
import com.protocoldsl.protocol.ValidationError

/**
 * Created by anders on 04/03/15.
 */

object MulOrEcho {
  def props() = Props(classOf[MulOrEcho])
}

case class AnInteger(n: Int)

class MulOrEcho extends Actor {

  def receive = {
    case AnInteger(n) =>
      context become mulServer
      self ! AnInteger(n)
    case data: String =>
      context become echoServer
      self.tell(data, sender())
    case Initiation =>
      println("Pm sent Initiation")
    case err@_ =>
      println(s"Unhandled message: $err")
      sender() ! ChildFinished
  }


  var isFirstNum = true
  var firstNum = 0

  def mulServer: Receive = {
    case AnInteger(n) if isFirstNum =>
      firstNum = n
      isFirstNum = !isFirstNum
    case AnInteger(n) =>
      println("Sending...")
      sender() ! ToConnection(s"${firstNum * n}\r\n")
      isFirstNum = !isFirstNum
    case ProtocolEnded(ValidationError(why, err)) =>
      sender() ! ToConnection(s"$why\r\n")
      sender() ! ChildFinished
    case err@_ =>
      // Send error message:
      println(s"Unhandled message: $err")
      sender() ! ChildFinished
  }

  def echoServer: Receive = {
    case data: String =>
      sender() ! ToConnection(data)
    case err@_ =>
      println(s"Closing because: $err")
      sender() ! ChildFinished
  }

  override def preStart(): Unit = {
    println("Staring...")
  }

  override def postStop(): Unit = {
    println("Stopping...")
  }
}