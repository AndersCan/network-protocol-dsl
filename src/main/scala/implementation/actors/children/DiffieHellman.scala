package implementation.actors.children

import akka.actor.{Actor, Props}
import akka.util.ByteString
import com.protocoldsl.actors._
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by aoc4 on 23/03/15.
 */

object DiffieHellman {
  def props() = Props(classOf[DiffieHellman])


}

class DiffieHellman extends Actor {

  var prime = 0.0
  val generator = 5.0
  // Generate a random Integer
  val privateKey = math.abs(scala.util.Random.nextInt(10) + 1)
  //  val privateKey = 15
  var myPublicKey = 0.0

  var sharedSecret = 0.0 // only A and B knows this

  var currentStep = "prime"

  val textEncryptor = new BasicTextEncryptor()

  override def receive: Receive = {
    case ToChildMessage(data) =>
      currentStep match {
        case "prime" =>
          println(s"PrivateKey: $privateKey")
          val receivedValue = data.toString.toDouble
          println(s"Prime: $receivedValue")
          prime = receivedValue
          myPublicKey = scala.math.pow(generator, privateKey) % prime
          println(s"Sending MyPubKey: $myPublicKey")
          sender() ! SendToConnection(ByteString.fromString(myPublicKey + "\r\n"))
          currentStep = "gotpubkey"
        case "gotpubkey" =>
          val receivedValue = data.toString.toDouble
          println(s"Received PubKey: $receivedValue")
          sharedSecret = scala.math.pow(receivedValue, privateKey) % prime
          println(s"Shared Secret: ($receivedValue^$privateKey) % $prime")
          println(s"Shared Secret: ${sharedSecret.toString}")
          textEncryptor.setPassword(sharedSecret.toString)
          currentStep = "secured"
        case "secured" =>
          //          val encrypted = textEncryptor.encrypt(data.toString)
          println(s"Encrypted message is ${data.toString}")
          println(s"Secret message is ${textEncryptor.decrypt(data.toString)}")
        //          println(s"sec: $sharedSecret")
        //          sender() ! SendToConnection(ByteString.fromString("server reply\r\n"))
        case _ => println(s"We got an unknown message: $data")
      }

    case ProtocolFailure(err) =>
      // Protocol has been ended. End self then tell parent
      println(err)
      sender() ! ChildFinished
    case Initiation =>
      // Do Initiation
      println("Server Initiation...")
    case unknown@_ => println(s"unknown message: $unknown")
  }
}