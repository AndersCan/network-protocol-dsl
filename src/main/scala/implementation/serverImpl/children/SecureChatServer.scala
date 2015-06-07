package implementation.serverImpl.children

import akka.actor.{Actor, ActorRef, Props}
import com.protocoldsl.actors._
import implementation.clientImpl.{Username, PubKey}
import net.liftweb.json._
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by aoc4 on 23/03/15.
 */

case class PrimeAndGenerator(prime: Double, generator: Double)

case class ChatMessage(from: String, to: String, msg: String)

case class SecureInit(from: String, header: String, number: Double)

case class ConnectedUsers(users: List[String])

case class NewUser(username: String)

case class SecureComInit(username: String, message: String)

case class DiffieInit(prime: Double)

case class EncryptedChatMessage(message: String)

object SecureChatServer {
  def props() = Props(classOf[SecureChatServer])
}

class SecureChatServer extends Actor {

  var prime = 0.0
  var generator = 2.0
  // Generate a random Integer
  val privateKey = math.abs(scala.util.Random.nextInt(10) + 1)
  //  val privateKey = 15
  var myPublicKey = 0.0

  var sharedSecret = 0.0 // only A and B knows this

  val textEncryptor = new BasicTextEncryptor()

  var username = ""

  var pm: ActorRef = null

  override def receive: Receive = {
    case Initiation =>
      // Do Initiation
      //println("Server Initiation...")
      // Store ProtocolMonitor for state ChatRoom
      pm = sender()
    case PrimeAndGenerator(p, g) =>
      prime = p
      generator = g
      myPublicKey = scala.math.pow(generator, privateKey) % prime
      //      println(s"Sending MyPubKey: $myPublicKey")
      sender() ! ToConnection( s""" { "publickey": "$myPublicKey" } """)
      context become WaitingForPubKey
    case err@_ => failure(err)
  }

  def WaitingForPubKey: Receive = {
    case PubKey(pk) =>
      //      println(s"Received PubKey: $receivedValue")
      sharedSecret = scala.math.pow(pk, privateKey) % prime
      //      println(s"Shared Secret: ($receivedValue^$privateKey) % $prime")
      //println(s"Shared Secret: ${sharedSecret.toString}")
      textEncryptor.setPassword(sharedSecret.toString)
      context become WaitingForUserName

    //      val listener = system.actorOf(Props(classOf[Listener], this))
    case err@_ => failure(err)
  }

  def WaitingForUserName: Receive = {
    case Username(encryptedusername) =>

      username = textEncryptor.decrypt(encryptedusername)
      println(s"New user connected: $username")
      context.system.eventStream.publish( s""" { "token" : "NewUser", "to": "ALL", "from" : "$username" } """)
      context become ChatRoom
      context.system.eventStream.subscribe(self, classOf[String])
    case err@_ => failure(err)
  }

  // React to published and received events
  def ChatRoom: Receive = {
    case EncryptedChatMessage(data) =>
      val decrypt: String = textEncryptor.decrypt(data)
      //      println(s"Secret message is $decrypt")
      context.system.eventStream.publish(decrypt)
    case input: String =>
      //      println(s"Raw: $input")
      val json = parse(input)
      //      println(s"Json: $json")
      implicit val formats = DefaultFormats
      val to = (json \ "to").extract[String]
      //      println(s"To: $to")
      if (to == username || to == "ALL") {
        //println(s"Sending encrypted message to: $username")
        pm ! ToConnection(encrypt(input))
      }

    case err@_ => failure(err)
  }

  def failure(msg: Any) = msg match {
    case ProtocolEnded(err) =>
      // Protocol has been ended. End self then tell parent
      println(err)
      context.system.eventStream.unsubscribe(self)
      sender() ! ChildFinished
    case unknown@_ =>
      println(s"Unspecified case for: $unknown")
      sender() ! ChildFinished
  }

  def encrypt(input: String): String = {
    textEncryptor.encrypt(input)
  }
}

