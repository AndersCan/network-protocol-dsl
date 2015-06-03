package implementation.clientImpl

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.{DelayedValidation, ProtocolMonitor}
import com.protocoldsl.protocol.{ProtocolBuilder, ValidationError, Validator}
import implementation.crypto.Helper
import implementation.serverImpl.children.NewUser
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
    val result = Helper.fermat(maybePrime)
    if (result) Right(input.toDouble)
    else Left(ValidationError("Not prime"))
  } catch {
    case e: Exception =>
      println(e.getMessage)
      Left(ValidationError("msg breaks protocol. not PRIME"))
  })

  val isDouble = new Validator(x => try {
    Right(x.dropRight(2).toDouble)
  } catch {
    case e: Exception =>
      Left(ValidationError("msg breaks protocol. not double"))
  })

  val username = new Validator(x => try {
    Right(x.dropRight(2))
  } catch {
    case e: Exception =>
      Left(ValidationError("msg breaks protocol. not a username"))
  })

  implicit val formats = DefaultFormats
  // No tests if value is prime
  val primeAndGenerator = new Validator(input => try {
    val json = parse(input)
    val p = json \ "prime"
    val g = json \ "generator"
    Right(PrimeAndGenerator(prime = p.extract[String].toDouble, generator = g.extract[String].toDouble))
  } catch {
    case e: Exception =>
      Left(ValidationError("Msg could not be converted to a PrimeAndGenerator class: ", e))
  })

  val aPublicKey = new Validator(input => try {
    val json = parse(input)
    val pk = json \ "publickey"
    // TODO FIX, PubKey class
    Right(PubKey(pk.extract[String].toDouble))
  } catch {
    case e: Exception =>
      Left(ValidationError("Msg could not be converted to a PubKey class: ", e))
  })

  val message = new Validator(x => try {
    val json = parse(x)
    val token = (json \ "token").extract[String] //.drop(1).dropRight(1)
    token.toString match {
      case "ConnectedUsers" =>
        Right(ConnectedUsers((json \ "users").children.map(_.toString)))
      case "SecureInit" =>
        Right(SecureInit((json \ "from").extract[String], (json \ "header").extract[String], (json \ "number").extract[String].toDouble))
      case "ChatMessage" =>
        Right(ChatMessage((json \ "from").extract[String], (json \ "to").extract[String], (json \ "msg").extract[String]))
      case "NewUser" =>
        Right(NewUser((json \ "from").extract[String]))
    }
  } catch {
    case e: Exception =>
      Left(ValidationError("Validation error on ChatMessage", e))
  })


  val aChatMessage = new Validator(x => try {
    Right(DelayedValidation(x, message))
  } catch {
    case e: Exception =>
      Left(ValidationError("Delayed Error on aChatMessage", e))
  })


  val secureCom = ProtocolBuilder() anyone isAnything loop()
  // next might be buggy
  val diffieClient = ProtocolBuilder() sends primeAndGenerator receives aPublicKey sends aPublicKey next secureCom
  //Diffie Initiation
  val diffieInit = ProtocolBuilder() sends primeAndGenerator receives aPublicKey sends aPublicKey
  //Diffie Server Chat
  // val diffieProtocol = diffieInit next ProtocolBuilder() anyone isAnything loop()

  //SecureChat
  // SERVER
  //val securechatProtocol = diffieInit receive username send usernames receive username next ProtocolBuilder() anyone isAnything loop()
  //CLIENT
  val securechatProtocol = diffieInit sends username looped(0, ProtocolBuilder() anyone aChatMessage loop())

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)
    case CommandFailed(_: Bind) => context stop self
    case cu@Connected(remote, local) =>
      //println(s"New Connection: remote: $remote, local: $local")
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