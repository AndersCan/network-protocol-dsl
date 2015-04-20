package implementation.clientImpl

import akka.actor._
import akka.util.ByteString
import com.protocoldsl.actors.{Initiation, SendToConnection, ToChildMessage}
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by anders on 16/04/15.
 */

object DiffieClient {
  def props() =
    Props(classOf[DiffieClient])
}

class DiffieClient() extends Actor {

  val prime: Double = BigInt.probablePrime(8, scala.util.Random).toDouble
  //  val prime = 23.0
  val generator = 5.0
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
      sender() ! SendToConnection(ByteString.fromString(prime.toString))
      currentStep = "pubkey"
      // wait for pubkey
      context become waitingForPubkey
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
      println(s"Shared Secret: $sharedSecret")
      textEncryptor.setPassword(sharedSecret.toString)
      context become secureCom
      // Start sending something
      self.tell(currentStep, sender())
  }

  def secureCom: Receive = {
    case _ =>
      sender() ! SendToConnection(sec("Hello From Client -- Secure Communication"))
    //      println(s"sec: $sharedSecret")
  }

  def sec(in: String): ByteString = {
    ByteString.fromString(textEncryptor.encrypt(in))
  }
}