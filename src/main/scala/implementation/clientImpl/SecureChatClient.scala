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

  var currentStep = "START"

  val textEncryptor = new BasicTextEncryptor()

  def receive = {
    case Initiation =>
      // send prime
      println(s"PrivateKey: $privateKey")
      println(s"Sending prime: $prime")
      //      sender() ! SendToConnection(ByteString.fromString(prime.toString))
      sender() ! SendToConnection(ByteString.fromString(s"$prime"))
      currentStep = "pubkey"
      // wait for pubkey
      context become waitingForPubkey
    case ProtocolFailure => sender() ! ChildFinished
    case _ =>
      println("Unknown message...")
  }

  def waitingForPubkey: Receive = {
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
      context become secureCom
      // Start sending something
      self.tell("START", sender())
      println("Hello....")
    case err@_ => failure(err)
  }

  def secureCom: Receive = {
    case "START" => sender() ! SendToConnection(sec("Starting communication..."))
    case ToChildMessage(data) =>
      val encrypted = sec("Hello From Client -- Secure Communication")
      //      println(encrypted)
      println(encrypted.utf8String)
      sender() ! SendToConnection(encrypted)
    //      println(s"sec: $sharedSecret")
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
    ByteString.fromString(textEncryptor.encrypt(in + "\n"))
  }
}