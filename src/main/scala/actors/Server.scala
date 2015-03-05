package actors

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props, Actor}
import akka.io.{IO, Tcp}
import protocol.Creator

/**
 * Created by anders on 04/03/15.
 */

object Server {
  def props(inetSocketAddress: InetSocketAddress) = Props(classOf[Server], inetSocketAddress)
}

class Server(inetSocketAddress: InetSocketAddress) extends Actor {

  import Tcp._
  import context.system

  //  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 0))
  IO(Tcp) ! Bind(self, inetSocketAddress)

  val proto = Creator.c.compile
  println(proto)

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)

    case CommandFailed(_: Bind) => context stop self

    case c@Connected(remote, local) =>
      val handler = context.actorOf(ProtocolHandler.props(proto))
      //      val handler = context.actorOf(Props[SimplisticHandler])
      // Sender() is sender of the current message
      val connection = sender()
      connection ! Register(handler)
  }

}