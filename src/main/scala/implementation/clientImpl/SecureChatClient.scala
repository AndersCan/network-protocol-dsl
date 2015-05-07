package implementation.clientImpl

import akka.actor._
import akka.util.ByteString
import com.protocoldsl.actors._
import implementation.actors.children.{DiffieInit, SecureComInit, NewUser}
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by anders on 16/04/15.
 */

case class PubKey(key: Double)

object SecureChatClient {
  def props() =
    Props(classOf[SecureChatClient])
}

class SecureChatClient() extends Actor {

  val prime: Double = BigInt.probablePrime(8, scala.util.Random).toDouble
  //  val prime = 23.0
  val GENERATOR = 2.0
  // Generate a random Integer
  val privateKey = math.abs(scala.util.Random.nextInt(10) + 1)
  //  val privateKey = 6
  var myPublicKey = 0.0

  var sharedSecret = 0.0 // only A and B knows this

  val textEncryptor = new BasicTextEncryptor()

  // Chat values
  val username = scala.math.abs(scala.util.Random.nextInt()).toString
  var prime2 = BigInt.probablePrime(8, scala.util.Random).toDouble
  val privateKey2 = math.abs(scala.util.Random.nextInt(10) + 1)
  var myPublicKey2 = 0.0
  // only A and C knows this
  var sharedSecret2 = 0.0
  var chatsecure = false
  val chatEncryptor = new BasicTextEncryptor()

  def receive = {
    case Initiation =>
      // send prime
      println(s"PrivateKey: $privateKey")
      println(s"Sending prime: $prime")
      sender() ! SendToConnection(ByteString.fromString(s"$prime"))
      context become WaitingForPubkey
    case ProtocolFailure => sender() ! ChildFinished
    case _ =>
      println("Unknown message...")
  }

  def WaitingForPubkey: Receive = {
    case ToChildMessage(data) =>
      val receivedValue = data.asInstanceOf[Double]
      //      println(s"Received PubKey: $receivedValue")
      myPublicKey = scala.math.pow(GENERATOR, privateKey) % prime
      //      println(s"Sending MyPubKey: $myPublicKey")
      sender() ! SendToConnection(ByteString.fromString(myPublicKey.toString))
      sharedSecret = scala.math.pow(receivedValue, privateKey) % prime
      //      println(s"Shared Secret: ($receivedValue^$privateKey) % $prime")
      //      println(s"Shared Secret: ${sharedSecret.toString}")
      textEncryptor.setPassword(sharedSecret.toString)
      // Send username
      context become ChatRoom
      val encryptedusername = sec("username;" + username)
      sender() ! SendToConnection(encryptedusername)
    case err@_ => failure(err)
  }

  def ChatRoom: Receive = {
    case ToChildMessage(data) =>
      getChatMessageType(textEncryptor.decrypt(data.toString)) match {
        case NewUser(name) =>
          println(s"Received username: $name")
          // Start DiffieInit

          sender() ! SendToConnection(sec(s"$username;diffie;$prime2"))
          myPublicKey2 = scala.math.pow(GENERATOR, privateKey2) % prime2
        case DiffieInit(p) =>
          // We are user 2
          println("We got DiffieInit!")
          prime2 = p
          myPublicKey2 = scala.math.pow(GENERATOR, privateKey2) % prime2
          println(s"Sending myPublickey: $myPublicKey2")
          sender() ! SendToConnection(sec(s"$username;pubkey;$myPublicKey2"))
        case PubKey(key) =>
          if (!chatsecure) {
            println(s"Received public key: $key")
            chatsecure = true
            sharedSecret2 = scala.math.pow(key, privateKey2) % prime2
            println(s"Shared Secret2: ($key^$privateKey2) % $prime2")
            println(s"Shared ChatSecret: ${sharedSecret2.toString}")
            chatEncryptor.setPassword(sharedSecret2.toString)
            sender() ! SendToConnection(sec(s"$username;pubkey;$myPublicKey2"))
          } else {
            println("Sending message...")
            sender() ! SendToConnection(sec2("message;This is a secure chat channel"))
          }
        case SecureComInit(user, msg) =>
          //          val decrypted = textEncryptor.decrypt(msg)
          //          println(s"Decrypted message: $decrypted")
          println(s"Received message: $msg from $user")
          println(s"Decrypted message: ${chatEncryptor.decrypt(msg)}")
      }
    case err@_ => failure(err)
  }

  def failure(msg: Any) = msg match {
    case ProtocolFailure(err) =>
      // Protocol has been ended. End self then tell parent
      println(err)
      sender() ! ChildFinished
    case unknown@_ => println(s"unknown message: $unknown")
  }

  // TODO Create Parent case class for chat message
  def getChatMessageType(msg: String): Any = {
    println(s"Msg Received: $msg")
    val split = msg.split(";")
    split(0) match {
      case "username" =>
        NewUser(split(1))
      case "message" =>
        SecureComInit(split(0), split(1))
      case "diffie" =>
        DiffieInit(split(1).toDouble)
      case "pubkey" =>
        PubKey(split(1).toDouble)
      case msg: String if msg forall Character.isDigit =>
        SecureComInit(split(0), split(1))
      case _ => Left("Unexpected ChatMessage received")
    }
  }

  def sec(in: String): ByteString = {
    ByteString.fromString(textEncryptor.encrypt(in + ""))
  }

  //Only to be used by SecureChat. Prepends username for Server to decrypt
  def sec2(in: String): ByteString = {
    ByteString.fromString(textEncryptor.encrypt(username + ';' + chatEncryptor.encrypt(in + "")))
  }
}