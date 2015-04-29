package implementation.actors

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.ProtocolMonitor
import com.protocoldsl.protocol.{Branch, ProtocolBuilder, Validator}
import implementation.actors.children.{IsInt, MulOrEcho}

/**
 * Created by anders on 04/03/15.
 */

object Server {
  def props(inetSocketAddress: InetSocketAddress) = Props(classOf[Server], inetSocketAddress)
}

class Server(inetSocketAddress: InetSocketAddress) extends Actor {

  import akka.io.Tcp._
  import context.system

  val isAnything = new Validator(in => Right(in))
  val nothing = new Validator(_ => Left("Nothing will always give a Left()"))

  val isInt = new Validator(x => try {
    Right(IsInt(x.dropRight(2).toInt))
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })

  val isDouble = new Validator(x => try {
    Right(x.dropRight(2).toDouble)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not double")
  })

  val isPrime = new Validator(input => try {
    val maybePrime: BigInt = BigInt(input.toString.dropRight(2))
    val result = com.protocoldsl.crypto.Helper.fermat(maybePrime)
    if (result) Right(input.toDouble)
    else Left("Not prime")
  } catch {
    case e: Exception =>
      println(e.getMessage)
      Left("msg breaks protocol. not Prime")
  })

  // Erlang concurrency model/patterns - network application tocols : struct proure of http, dns. Transport Layer Security, SSL
  // concurrent protocol/programming patterns
  // implement kerberos. modularity. optional actors
  // Secure ChatRoom
  // TODO - How to add multiple communication channels? {S :: C :: C}
  implicit val server = new ProtocolBuilder()

  val mulServer =
    server receive isInt receive isInt send isAnything loop()

  val echoServer = ProtocolBuilder.loop(
    server receive isAnything send isAnything
  )

  val mulOrEchoTest = new Branch(input =>
    if (input forall Character.isDigit) mulServer
    else echoServer
  )

  val branching = ProtocolBuilder() branchOn mulOrEchoTest

  //Diffie
  val diffie = server receive isPrime send isDouble receive isDouble looped(0, server receive isAnything send isAnything loop())

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
      val consumer = context.actorOf(MulOrEcho.props())
      // Sender() is sender of the current message
      val connection = sender()
      val handler = context.actorOf(ProtocolMonitor.props(proto, connection, consumer))
      connection ! Register(handler)
  }

  override def preStart(): Unit = {
    IO(Tcp) ! Bind(self, inetSocketAddress)
  }

}