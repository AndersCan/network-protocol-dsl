package actors

import java.net.InetSocketAddress

import actors.children.{DiffieHellman, SimplisticHandler}
import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import protocol.{ProtocolBuilder, Validator}

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

  val isAnything = new Validator(_ => Right(true))
  val isInt = new Validator(x => try {
    // Remove \n from end of line
    x.dropRight(2).toInt
    Right(true)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })
  val isDouble = new Validator(x => try {
    // Remove \n from end of line
    x.dropRight(2).toDouble
    Right(true)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })


  // TODO - How to add multiple communication channels? {S :: C :: C}
  val c = new ProtocolBuilder()
  val s = new ProtocolBuilder()

  // ECHO SERVER
  //  c send(s, isInt)
  //  c send(s, isInt)
  //  s send(c, new Validator(_ => Right(true))) // String
  //  c gotoStep 0
  // END ECHO SERVER

  //Diffie

  c send(s, isDouble) // sends prime
  s send(c, isDouble) // sends shared secret
  c send(s, isDouble) // sends shared secret
  c send(s, isAnything)
  s send(c, isAnything)
  c gotoStep 3


  //  val proto = c.compile
  val proto = s.compile

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)

    case CommandFailed(_: Bind) => context stop self

    case c@Connected(remote, local) =>
      val diffie = context.actorOf(DiffieHellman.props())
      //      val child = context.actorOf(SimplisticHandler.props())
      // Sender() is sender of the current message
      val connection = sender()
      val handler = context.actorOf(ProtocolMaster.props(proto, connection, diffie))
      //      val handler = context.actorOf(Props[SimplisticHandler])
      connection ! Register(handler)
  }

}