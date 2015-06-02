package implementation.clientImpl

import akka.actor._
import com.protocoldsl.actors._
import implementation.clientImpl.util.{DiffieHellman, Prime, PublicKey, StartDiffie}
import implementation.serverImpl.children.{EncryptedChatMessage, NewUser}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by anders on 16/04/15.
 */

case class PrimeAndGenerator(prime: Double, generator: Double)

case class PubKey(key: Double)

case class Username(username: String)


object SecureChatClient {
  def props() =
    Props(classOf[SecureChatClient])
}

class SecureChatClient() extends Actor {

  var pm: ActorRef = null
  val prime: Double = BigInt.probablePrime(8, scala.util.Random).toDouble
  //  val prime = 23.0
  val generator = 2.0
  // Generate a random Integer
  val privateKey = math.abs(scala.util.Random.nextInt(10) + 1)
  //  val privateKey = 6
  var myPublicKey = 0.0

  var sharedSecret = 0.0 // only A and B knows this

  val textEncryptor = new BasicTextEncryptor()

  // Chat values
  val username = scala.math.abs(scala.util.Random.nextInt()).toString
  //  var prime2 = BigInt.probablePrime(8, scala.util.Random).toDouble
  //  val privateKey2 = math.abs(scala.util.Random.nextInt(10) + 1)
  //  var myPublicKey2 = 0.0
  // only A and C knows this
  //  var sharedSecret2 = 0.0
  //  var chatsecure = false
  //  var user1 = false
  //  val chatEncryptor = new BasicTextEncryptor()

  // Map of ChatRoom Users
  var connectedUsers: Map[String, ActorRef] = Map()

  def receive = {
    case Initiation =>
      pm = sender()
      // send prime
      println(s"PrivateKey: $privateKey")
      println(s"Sending prime: $prime and generator $generator")
      pm ! SendToConnection( s"""{ "prime" : "$prime", "generator" : "$generator" } """)
      context become WaitingForPubkey
    case ProtocolEnded => sender() ! ChildFinished
    case err@_ =>
      println(s"SCC: Unknown message...$err")
  }

  def WaitingForPubkey: Receive = {
    case PubKey(pk) =>
      myPublicKey = scala.math.pow(generator, privateKey) % prime
      //      println(s"Sending MyPubKey: $myPublicKey")
      sender() ! SendToConnection( s""" { "publickey": "$myPublicKey" } """)
      sharedSecret = scala.math.pow(pk, privateKey) % prime
      //      println(s"Shared Secret: ($receivedValue^$privateKey) % $prime")
      //      println(s"Shared Secret: ${sharedSecret.toString}")
      textEncryptor.setPassword(sharedSecret.toString)
      // Send username
      context become ChatRoom
      val encryptedusername = encrypt(username)
      sender() ! SendToConnection(encryptedusername)
    case err@_ => failure(err)
  }


  def ChatRoom: Receive = {
    case EncryptedChatMessage(message) =>
      // Decrypts with server key
      println("Result Received: " + getChatMessageType(textEncryptor.decrypt(message)))
      getChatMessageType(textEncryptor.decrypt(message)) match {
        case NewUser(name) =>
          // Add new user to our map
          println("Sending StartDiffie...")
          val diffie = context.actorOf(DiffieHellman.props(username, name))
          diffie ! StartDiffie()
          connectedUsers += name -> diffie
        case SecureInit(from, header, number) =>
          header match {
            case "Prime" =>
              // We are user 2
              // Create Actor for this connection
              val diffie = context.actorOf(DiffieHellman.props(username, from))
              diffie ! Prime(number)
              connectedUsers += from -> diffie
            case "PubKey" =>
              connectedUsers(from) ! PublicKey(number)
          }
        case cm@ChatMessage(from, to, msg) =>
          connectedUsers(from) ! cm
      }
    case SendToConnection(body) => pm ! SendToConnection(encrypt(body))
    case err@_ => failure(err)
  }

  def failure(msg: Any) = msg match {
    case ProtocolEnded(err) =>
      // Protocol has been ended. End self then tell parent
      println(err)
      sender() ! ChildFinished
    case unknown@_ => println(s"Unimplemented case: $unknown")
  }

  implicit val formats = DefaultFormats

  // TODO Create Parent case class for chat message
  def getChatMessageType(msg: String): Any = {
    //    println(s"Raw: $msg")
    val json = parse(msg)
    val token = compact(render(json \ "token")).extract[String].drop(1).dropRight(1)
    println(s"Token: $token")
    token.toString match {
      case "ConnectedUsers" =>
        ConnectedUsers((json \ "users").children.map(_.toString))
      case "SecureInit" =>
        SecureInit((json \ "from").extract[String], (json \ "header").extract[String], (json \ "number").extract[String].toDouble)
      case "ChatMessage" =>
        ChatMessage((json \ "from").extract[String], (json \ "to").extract[String], (json \ "msg").extract[String])
      case "NewUser" =>
        NewUser((json \ "from").extract[String])
      case _ => Left("Unexpected ChatMessage received")
    }
  }

  def encrypt(in: String): String = {
    textEncryptor.encrypt(in)
  }
}