package implementation.serverImpl.performance

package implementation.serverImpl

import java.net.InetSocketAddress

import akka.actor.{PoisonPill, Actor, Props}
import akka.io.{IO, Tcp}

/**
 * Created by anders on 04/03/15.
 */

object EchoServer {
  def props(inetSocketAddress: InetSocketAddress) = Props(classOf[EchoServer], inetSocketAddress)
}

class EchoServer(inetSocketAddress: InetSocketAddress) extends Actor {

  import akka.io.Tcp._
  import context.system



  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("PERFORMANCE ECHO SERVER bound to " + localAddress)

    case CommandFailed(_: Bind) => context stop self

    case cu@Connected(remote, local) =>
      println(s"New Connection: remote: $remote, local: $local")
      // Sender() is sender of the current message
      val connection = sender()
      val handler = context.actorOf(Props[SimplisticHandler])
      connection ! Register(handler)
  }

  override def preStart(): Unit = {
    IO(Tcp) ! Bind(self, inetSocketAddress)
  }

}

/**
 * SimplisticHandler has no checking
 */
class SimplisticHandler extends Actor {
  import Tcp._
  def receive = {
    case Received(data) =>
//      println("Server Got data...")
      sender() ! Write(data)
    case PeerClosed     => context stop self
    case ErrorClosed(err) =>
      println("Connection closed")
      self ! PoisonPill
    case err@_ => println("unknown msg: " + err)
  }

  override def preStart(): Unit = {
    println("Starting...")
  }
}