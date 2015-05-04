package implementation.clientImpl

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.ProtocolMonitor
import com.protocoldsl.protocol.{ProtocolBuilder, Validator}

/**
 * Created by anders on 20/04/15.
 */

object ClientServer {
  def props(remotHost: InetSocketAddress) = Props(classOf[ClientServer], remotHost)
}

class ClientServer(remoteHost: InetSocketAddress) extends Actor {

  import akka.io.Tcp._
  import context.system


  val isPrime = new Validator(input => try {
    val maybePrime: BigInt = BigInt(input.toString.dropRight(2))
    println(maybePrime)
    val result = com.protocoldsl.crypto.Helper.fermat(maybePrime)
    if (result) Right(input.toDouble)
    else Left("Not prime")
  } catch {
    case e: Exception =>
      println(e.getMessage)
      Left("msg breaks protocol. not PRIME")
  })

  val isDouble = new Validator(x => try {
    Right(x.dropRight(2).toDouble)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not double")
  })

  val isAnything = new Validator(in => Right(in))

  val secureCom = ProtocolBuilder() send isAnything receive isAnything loop()

  val diffieClient = ProtocolBuilder() send isPrime receive isDouble send isDouble next secureCom


  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)
    case CommandFailed(_: Bind) => context stop self
    case cu@Connected(remote, local) =>
      println(s"New Connection: remote: $remote, local: $local")
      val proto = diffieClient.compile
      val consumer = context.actorOf(DiffieHellmanClient.props())
      // Sender() is sender of the current message
      val connection = sender()
      val pm = context.actorOf(ProtocolMonitor.props(proto, connection, consumer))
      connection ! Register(pm)
    case _ => println("WHAT?")
  }

  override def preStart(): Unit = {
    IO(Tcp) ! Connect(remoteHost)
  }
}