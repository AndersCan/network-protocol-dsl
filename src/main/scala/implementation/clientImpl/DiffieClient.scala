package implementation.clientImpl

import java.net.InetSocketAddress

import akka.actor._
import akka.io.{IO, Tcp}
import akka.util.ByteString

import scala.concurrent.duration._

/**
 * Created by anders on 16/04/15.
 */

object DiffieClient {
  def props(remote: InetSocketAddress) =
    Props(classOf[DiffieClient], remote)
}

class DiffieClient(remoteHost: InetSocketAddress) extends Actor {

  import Tcp._
  import context.system

  var connection: ActorRef = null

  def receive = {
    case CommandFailed(_: Connect) =>
      context stop self
    case c@Connected(remote, local) =>
      connection = sender()
      connection ! Register(self)
      self ! "send"
      context become connected
    case CommandFailed(w: Write) =>
    // O/S buffer was full
    case "close" =>
      connection ! Close
    case _: ConnectionClosed =>
      context stop self
    case _ =>
      println("Unknown message")
  }

  def connected: Receive = {
    case Received(data) =>
      println(s"Received: ${data.utf8String}")
      connection ! Tcp.Write(ByteString.fromString("Client reply\r\n"))
    case "send" =>
      println("Sending msg to server...")
      connection ! Tcp.Write(ByteString.fromString("Client reply\r\n"))
  }

  override def preStart() {
    IO(Tcp) ! Connect(remoteHost)
  }
}