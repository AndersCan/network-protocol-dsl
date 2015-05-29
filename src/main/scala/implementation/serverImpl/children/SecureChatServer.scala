package implementation.serverImpl.children

import akka.actor.{Actor, ActorRef, Props}
import akka.util.ByteString
import com.protocoldsl.actors._
import net.liftweb.json._
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by aoc4 on 23/03/15.
 */

case class ChatMessage(from: String, to: String, msg: String)

case class SecureInit(from: String, header: String, number: Double)

case class ConnectedUsers(users: List[String])

case class NewUser(username: String)

case class SecureComInit(username: String, message: String)

case class DiffieInit(prime: Double)

object SecureChatServer {
  def props() = Props(classOf[SecureChatServer])
}

class SecureChatServer extends Actor {

  var prime = 0.0
  val generator = 2.0
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
      println("Server Initiation...")
      // Store ProtocolMonitor for state ChatRoom
      pm = sender()
    case ToChildMessage(data) =>
      //      println(s"PrivateKey: $privateKey")
      val receivedValue = data.toString.toDouble
      //      println(s"Prime: $receivedValue")
      prime = receivedValue
      myPublicKey = scala.math.pow(generator, privateKey) % prime
      //      println(s"Sending MyPubKey: $myPublicKey")
      sender() ! SendToConnection(ByteString.fromString(myPublicKey + "\r\n"))
      context become WaitingForPubKey
    case err@_ => failure(err)
  }

  def WaitingForPubKey: Receive = {
    case ToChildMessage(data) =>
      val receivedValue = data.toString.toDouble
      //      println(s"Received PubKey: $receivedValue")
      sharedSecret = scala.math.pow(receivedValue, privateKey) % prime
      //      println(s"Shared Secret: ($receivedValue^$privateKey) % $prime")
      println(s"Shared Secret: ${sharedSecret.toString}")
      textEncryptor.setPassword(sharedSecret.toString)
      context become WaitingForUserName

    //      val listener = system.actorOf(Props(classOf[Listener], this))
    case err@_ => failure(err)
  }

  def WaitingForUserName: Receive = {
    case ToChildMessage(data) =>
      // username;<value>
      username = textEncryptor.decrypt(data.toString).split(';')(1)
      println(s"Connected user is $username")
      context.system.eventStream.publish( s""" { "token" : "NewUser", "to": "ALL", "from" : "$username" } """)
      context become ChatRoom
      context.system.eventStream.subscribe(self, classOf[String])
    //      context.system.eventStream.subscribe(self, classOf[SecureComInit])
    //      context.system.eventStream.subscribe(self, classOf[ChatMessage])
    case err@_ => failure(err)
  }

  // React to published and received events
  def ChatRoom: Receive = {
    //    case NewUser(name) =>
    //      // a new user has joined
    //      pm ! SendToConnection(sec("username;" + name))
    //    case ChatMessage(name, message) =>
    //      println(s"Sending message RIGHT: $name;$message to user $username")
    //      pm ! SendToConnection(sec(s"$name;$message"))
    //    case SecureComInit(name, message) =>
    //      if (username != name) {
    //        println(s"Sending message WRONG: $message to user $username")
    //        pm ! SendToConnection(sec(message))
    //      }
    case ToChildMessage(data) =>
      // TODO Cleanup of sending messages, wasteful split. Perhaps swap to JSON
      val decrypt: String = textEncryptor.decrypt(data.toString)
      //      println(s"Secret message is $decrypt")
      context.system.eventStream.publish(decrypt)
    case input: String =>
      println(s"Raw: $input")
      val json = parse(input)
      //      println(s"Json: $json")
      implicit val formats = DefaultFormats
      val to = (json \ "to").extract[String]
      //      println(s"To: $to")
      if (to == username || to == "ALL") {
        //        println("Sending...")
        pm ! SendToConnection(sec(input))
      }
    //      val json = parse(decrypt)
    //      (json \ "token").toString match {
    //        case "SecureInit" =>
    //          context.system.eventStream.publish(SecureInit())
    //      } else if (decrypt.contains(";pubkey;")) {
    //      val split = decrypt.split(";pubkey;")
    //      context.system.eventStream.publish(SecureComInit(split(0), "pubkey;" + split(1)))
    //    } else {
    //      val split = decrypt.split(";")
    //      context.system.eventStream.publish(SecureComInit(split(0), split(1)))
    //    }
    case err@_ => failure(err)
  }

  def failure(msg: Any) = msg match {
    case ProtocolEnded(err) =>
      // Protocol has been ended. End self then tell parent
      println(err)
      context.system.eventStream.unsubscribe(self)
      sender() ! ChildFinished
    case unknown@_ => println(s"unknown message: $unknown")
  }

  def sec(in: String): ByteString = {
    ByteString.fromString(textEncryptor.encrypt(in + ""))
  }
}

