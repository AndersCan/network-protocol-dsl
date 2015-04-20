package implementation.actors.children

import akka.actor.{Actor, Props}
import akka.util.ByteString
import com.protocoldsl.actors.{ChildFinished, ProtocolFailure, SendToConnection, ToChildMessage}
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by aoc4 on 23/03/15.
 */

object DiffieHellman {
  def props() = Props(classOf[DiffieHellman])


}

class DiffieHellman extends Actor {

  var prime = 0.0
  val generator = 3.0
  // Generate a random Integer
  val privateKey = math.abs(scala.util.Random.nextInt(100) + 1)
  var myPublicKey = 0.0

  var sharedSecret = 0.0 // only A and B knows this

  var currentStep = "prime"

  val textEncryptor = new BasicTextEncryptor()

  override def receive: Receive = {
    case ToChildMessage(data) =>
      currentStep match {
        case "prime" =>
          val receivedValue = data.asInstanceOf[Double]
          prime = receivedValue
          println(s"($generator ^ $privateKey) % $prime")
          myPublicKey = scala.math.pow(generator, privateKey) % prime
          sender() ! SendToConnection(ByteString.fromString(myPublicKey + "\r\n"))
          println(s"myPubK: $myPublicKey")
          currentStep = "gotpubkey"
        case "gotpubkey" =>
          val receivedValue = data.asInstanceOf[Double]
          sharedSecret = scala.math.pow(receivedValue, privateKey) % prime
          println(s"($receivedValue ^ $privateKey) % $prime")
          textEncryptor.setPassword(sharedSecret.toString)
          println(s"Shared secret is $sharedSecret")
          currentStep = "secured"
        case "secured" =>
          val encrypted = textEncryptor.encrypt(data.toString)
          println(s"Encrypted is $encrypted")
          println(s"MessageMessage is ${textEncryptor.decrypt(encrypted)}")
          sender() ! SendToConnection(ByteString.fromString("server reply\r\n"))
        case _ => println(s"We got an unknown message: $data")
      }

    case ProtocolFailure(err) =>
      // Protocol has been ended. End self then tell parent
      println(err)
      sender() ! ChildFinished
    case _ => println("unknown message")
  }
}