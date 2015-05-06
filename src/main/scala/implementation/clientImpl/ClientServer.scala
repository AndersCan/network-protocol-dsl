package implementation.clientImpl

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.ProtocolMonitor
import com.protocoldsl.protocol.{ProtocolBuilder, Validator}
import implementation.actors.children.{ChatMessage, NewUser}

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

  val username = new Validator(x => try {
    Right(x.dropRight(2))
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not a username")
  })

  val usernames = new Validator(x => try {
    // List of usernames
    Right(x.dropRight(2))
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not a username")
  })

  val isAnything = new Validator(in => Right(in))


  val chatMessage = new Validator(msg => try {
    println(s"Message: $msg")
    val split = msg.split(";")
    split(0) match {
      case "username" =>
        Right(NewUser(split(1)))
      case "message" =>
        Right(ChatMessage(split(1)))
      case _ => Left("Unexpected ChatMessage received")
    }
  } catch {
    case e: Exception =>
      Left(s"msg breaks protocol. Exception caught $e")
  })


  val secureCom = ProtocolBuilder() anyone isAnything loop()

  val diffieClient = ProtocolBuilder() send isPrime receive isDouble send isDouble next secureCom
  //Diffie Initiation
  val diffieInit = ProtocolBuilder() send isPrime receive isDouble send isDouble
  //Diffie Server Chat
  val diffieProtocol = diffieInit next ProtocolBuilder() anyone isAnything loop()

  //SecureChat
  // SERVER
  //val securechatProtocol = diffieInit receive username send usernames receive username next ProtocolBuilder() anyone isAnything loop()
  //CLIENT
  val securechatProtocol = diffieInit send username looped(0, ProtocolBuilder() anyone isAnything loop())

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)
    case CommandFailed(_: Bind) => context stop self
    case cu@Connected(remote, local) =>
      println(s"New Connection: remote: $remote, local: $local")
      val proto = securechatProtocol.compile
      val consumer = context.actorOf(SecureChatClient.props())
      // Sender() is sender of the current message
      val connection = sender()
      val pm = context.actorOf(ProtocolMonitor.props(proto, connection, consumer))
      connection ! Register(pm)
    case _ => println("ClientServer got an unexpected message")
  }

  override def preStart(): Unit = {
    IO(Tcp) ! Connect(remoteHost)
  }
}