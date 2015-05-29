package implementation.clientImpl

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.ProtocolMonitor
import com.protocoldsl.protocol.{ProtocolBuilder, Validator}
import implementation.serverImpl.children.{SecureComInit, NewUser}
import net.liftweb.json._

/**
 * Created by anders on 20/04/15.
 */

case class ChatMessage(from: String, to: String, msg: String)

case class SecureInit(from: String, header: String, number: Double)

case class ConnectedUsers(users: List[String])

object ClientServer {
  def props(remoteHost: InetSocketAddress) = Props(classOf[ClientServer], remoteHost)
}

class ClientServer(remoteHost: InetSocketAddress) extends Actor {

  import akka.io.Tcp._
  import context.system


  val isAnything = new Validator(in => Right(in))

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


  val Message = new Validator(x => try {
    println(s"JSON: $x")
    val json = parse(x)
    println(s"Token: $json")
    val token = (json \ "token").toString.drop(1).dropRight(1)
    token.toString match {
      case "ConnectedUsers" =>
        Right(ConnectedUsers((json \ "users").children.map(_.toString)))
      case "SecureInit" =>
        Right(SecureInit((json \ "from").toString, (json \ "header").toString, (json \ "msg").toString.toDouble))
      case "ChatMessage" =>
        Right(ChatMessage((json \ "from").toString, (json \ "to").toString, (json \ "msg").toString))
      case "NewUser" =>
        Right(NewUser((json \ "from").toString))
    }
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not a valid message")
  })


  //  val chatMessage = new Validator(msg => try {
  //    println(s"Message: $msg")
  //    val split = msg.split(";")
  //    split(0) match {
  //      case "username" =>
  //        Right(NewUser(split(1)))
  //      case "message" =>
  //        Right(ChatMessage(split(1)))
  //      case _ => Left("Unexpected ChatMessage received")
  //    }
  //  } catch {
  //    case e: Exception =>
  //      Left(s"msg breaks protocol. Exception caught $e")
  //  })


  val secureCom = ProtocolBuilder() anyone isAnything loop()

  val diffieClient = ProtocolBuilder() sends isPrime receives isDouble sends isDouble next secureCom
  //Diffie Initiation
  val diffieInit = ProtocolBuilder() sends isPrime receives isDouble sends isDouble
  //Diffie Server Chat
  val diffieProtocol = diffieInit next ProtocolBuilder() anyone isAnything loop()

  //SecureChat
  // SERVER
  //val securechatProtocol = diffieInit receive username send usernames receive username next ProtocolBuilder() anyone isAnything loop()
  //CLIENT
  val securechatProtocol = diffieInit sends username looped(0, ProtocolBuilder() anyone isAnything loop())

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