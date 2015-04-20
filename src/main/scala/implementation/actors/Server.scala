package implementation.actors

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.ProtocolMonitor
import com.protocoldsl.protocol.{Branch, ProtocolBuilder, Validator}
import implementation.actors.children.{DiffieHellman, MulOrEcho}

/**
 * Created by anders on 04/03/15.
 */

object Server {
  def props(inetSocketAddress: InetSocketAddress) = Props(classOf[Server], inetSocketAddress)
}

class Server(inetSocketAddress: InetSocketAddress) extends Actor {

  import akka.io.Tcp._
  import context.system

  IO(Tcp) ! Bind(self, inetSocketAddress)

  val isAnything = new Validator(_ => Right(true))
  val nothing = new Validator(_ => Left("Nothing will always give a Left()"))

  val isInt = new Validator(x => try {
    println("This must be an Int")
    x.toInt
    Right(true)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })

  val isDouble = new Validator(x => try {
    // Remove \n from end of line
    x.toDouble
    Right(true)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not double")
  })

  val isPrime = new Validator(input => try {
    val maybePrime: BigInt = BigInt(input.toString)
    println(maybePrime)
    val result = com.protocoldsl.crypto.Helper.fermat(maybePrime)
    println(result)
    if (result) Right(true)
    else Left("Not prime")
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })


  // TODO - How to add multiple communication channels? {S :: C :: C}
  implicit val server = new ProtocolBuilder()

  val mulServer =
    server receive isInt receive isInt send isAnything looped(1, server receive isAnything loop())

  val echoServer = ProtocolBuilder.loop(
    server receive isAnything send isAnything
  )

  val mulOrEchoTest = new Branch(input =>
    if (input forall Character.isDigit) mulServer
    else echoServer
  )

  val branching = ProtocolBuilder() branchOn mulOrEchoTest


  //Diffie
  val diffi = server receive isPrime send isDouble receive isDouble looped(0, server receive isAnything send isAnything loop())

  //    c send(s, isPrime) // sends prime
  //    s send(c, isDouble) // sends shared secret
  //    c send(s, isDouble) // sends shared secret
  //    c send(s, isAnything)
  //    s send(c, isAnything)
  //    s gotoStep 3

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)

    case CommandFailed(_: Bind) => context stop self

    case cu@Connected(remote, local) =>
      println(s"New Connection: remote: $remote, local: $local")
      val proto = echoServer.compile
      val child = context.actorOf(MulOrEcho.props())
      // Sender() is sender of the current message
      val connection = sender()
      val handler = context.actorOf(ProtocolMonitor.props(proto, connection, child))
      connection ! Register(handler)
  }
}