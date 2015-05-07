package implementation.actors.children

import akka.actor.{Actor, ActorRef, Props}
import akka.util.ByteString
import com.protocoldsl.actors._
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by aoc4 on 23/03/15.
 */



case class NewUser(username: String)

case class SecureComInit(username: String, message: String)

case class ChatMessage(username: Integer, message: String)

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
      context.system.eventStream.publish(NewUser(username))
      context become ChatRoom
      context.system.eventStream.subscribe(self, classOf[NewUser])
      context.system.eventStream.subscribe(self, classOf[SecureComInit])
      context.system.eventStream.subscribe(self, classOf[ChatMessage])
    case err@_ => failure(err)
  }

  // React to published and received events
  def ChatRoom: Receive = {
    case NewUser(name) =>
      // a new user has joined
      pm ! SendToConnection(sec("username;" + name))
    case ChatMessage(name, message) =>
      println(s"Sending message RIGHT: $name;$message to user $username")
      pm ! SendToConnection(sec(s"$name;$message"))
    case SecureComInit(name, message) =>
      if (username != name) {
        println(s"Sending message WRONG: $message to user $username")
        pm ! SendToConnection(sec(message))
      }

    case ToChildMessage(data) =>
      // TODO Cleanup of sending messages, wasteful split. Perhaps swap to JSON
      val decrypt: String = textEncryptor.decrypt(data.toString)
      println(s"Secret message is $decrypt")
      if (decrypt.contains(";diffie;")) {
        // <username>;diffie;<msg>
        val split = decrypt.split(";diffie;")
        context.system.eventStream.publish(SecureComInit(split(0), "diffie;" + split(1)))
      } else if (decrypt.contains(";pubkey;")) {
        val split = decrypt.split(";pubkey;")
        context.system.eventStream.publish(SecureComInit(split(0), "pubkey;" + split(1)))
      } else {
        val split = decrypt.split(";")
        context.system.eventStream.publish(SecureComInit(split(0), split(1)))
      }
    case err@_ => failure(err)
  }

  def failure(msg: Any) = msg match {
    case ProtocolFailure(err) =>
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

