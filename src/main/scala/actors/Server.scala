package actors

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import protocol.{Socket, Validator}

/**
 * Created by anders on 04/03/15.
 */

object Server {
  def props(inetSocketAddress: InetSocketAddress) = Props(classOf[Server], inetSocketAddress)
}

class Server(inetSocketAddress: InetSocketAddress) extends Actor {

  import akka.io.Tcp._
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
  c send(s, isInt)
  s send(c, new Validator(_ => Right(true))) // String
  c gotoStep 0



  val proto = c.compile

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)

    case CommandFailed(_: Bind) => context stop self

    case c@Connected(remote, local) =>
      val child = context.actorOf(SimplisticHandler.props())
      // Sender() is sender of the current message
      val connection = sender()
      val handler = context.actorOf(ProtocolHandler.props(proto, connection, child))
      //      val handler = context.actorOf(Props[SimplisticHandler])
      connection ! Register(handler)
  }

}