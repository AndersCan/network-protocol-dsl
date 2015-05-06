package implementation.clientImpl

import akka.actor._
import akka.util.ByteString
import com.protocoldsl.actors._
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by anders on 16/04/15.
 */

object SecureChatClient {
  def props() =
    Props(classOf[SecureChatClient])
}

class SecureChatClient() extends Actor {

  val prime: Double = BigInt.probablePrime(8, scala.util.Random).toDouble
  //  val prime = 23.0
  val generator = 2.0
  // Generate a random Integer
  val privateKey = math.abs(scala.util.Random.nextInt(10) + 1)
  //  val privateKey = 6
  var myPublicKey = 0.0

  var sharedSecret = 0.0 // only A and B knows this


  val textEncryptor = new BasicTextEncryptor()

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
      println(s"Received PubKey: $receivedValue")
      myPublicKey = scala.math.pow(generator, privateKey) % prime
      println(s"Sending MyPubKey: $myPublicKey")
      sender() ! SendToConnection(ByteString.fromString(myPublicKey.toString))
      sharedSecret = scala.math.pow(receivedValue, privateKey) % prime
      println(s"Shared Secret: ($receivedValue^$privateKey) % $prime")
      println(s"Shared Secret: ${sharedSecret.toString}")
      textEncryptor.setPassword(sharedSecret.toString)
      // Send username
      context become ChatRoom
      val sending = sec(scala.util.Random.nextInt().toString)
      println(s"Sending: ${sending.utf8String}")
      sender() ! SendToConnection(sending)
    case err@_ => failure(err)
  }

  def ChatRoom: Receive = {
    case "START" =>
      // Register for
      sender() ! SendToConnection(sec("Starting communication..."))
    case ToChildMessage(data) =>

      val decrypted = textEncryptor.decrypt(data.toString)
      println(s"Decrypted message: $decrypted")
    case err@_ => failure(err)
  }

  def failure(msg: Any) = msg match {
    case ProtocolFailure(err) =>
      // Protocol has been ended. End self then tell parent
      println(err)
      sender() ! ChildFinished
    case unknown@_ => println(s"unknown message: $unknown")
  }

  def sec(in: String): ByteString = {
    ByteString.fromString(textEncryptor.encrypt(in + ""))
  }
}