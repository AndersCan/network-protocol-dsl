package actors

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props, Actor}
import akka.io.{IO, Tcp}
import protocol.{Validator, Socket, Creator}

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

  val isInt = new Validator(x => try {
    // Remove \n from end of line
    x.dropRight(2).toInt
    Right(true)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })

  val c = new Socket()
  val s = new Socket()

  c send(s, isInt)
  s send(c, isInt)

  val proto = c.compile

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)

    case CommandFailed(_: Bind) => context stop self

    case c@Connected(remote, local) =>
      val child = context.actorOf(SimplisticHandler.props())
      val handler = context.actorOf(ProtocolHandler.props(proto, child))
      //      val handler = context.actorOf(Props[SimplisticHandler])
      // Sender() is sender of the current message
      val connection = sender()
      connection ! Register(handler)
  }

}