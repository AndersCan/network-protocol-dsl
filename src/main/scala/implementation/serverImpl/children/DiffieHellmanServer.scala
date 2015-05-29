package implementation.serverImpl.children

import akka.actor.{Actor, Props}
import akka.util.ByteString
import com.protocoldsl.actors._
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by aoc4 on 23/03/15.
 */

object DiffieHellmanServer {
  def props() = Props(classOf[DiffieHellmanServer])


}

class DiffieHellmanServer extends Actor {

  var prime = 0.0
  val generator = 2.0
  // Generate a random Integer
  val privateKey = math.abs(scala.util.Random.nextInt(10) + 1)
  //  val privateKey = 15
  var myPublicKey = 0.0

  var sharedSecret = 0.0 // only A and B knows this


  val textEncryptor = new BasicTextEncryptor()

  override def receive: Receive = {
    case Initiation =>
      // Do Initiation
      println("Server Initiation...")
    case ToChildMessage(data) =>
      println(s"PrivateKey: $privateKey")
      val receivedValue = data.toString.toDouble
      println(s"Prime: $receivedValue")
      prime = receivedValue
      myPublicKey = scala.math.pow(generator, privateKey) % prime
      println(s"Sending MyPubKey: $myPublicKey")
      sender() ! SendToConnection(ByteString.fromString(myPublicKey + "\r\n"))
      context become WaitingForPubKey
    case err@_ => failure(err)
  }

  def WaitingForPubKey: Receive = {
    case ToChildMessage(data) =>
      val receivedValue = data.toString.toDouble
      println(s"Received PubKey: $receivedValue")
      sharedSecret = scala.math.pow(receivedValue, privateKey) % prime
      println(s"Shared Secret: ($receivedValue^$privateKey) % $prime")
      println(s"Shared Secret: ${sharedSecret.toString}")
      textEncryptor.setPassword(sharedSecret.toString)
      context become Secure
      self.tell(ToChildMessage(textEncryptor.encrypt("hello")), sender())
    case err@_ => failure(err)
  }

  def Secure: Receive = {
    case ToChildMessage(data) =>
      println(s"Encrypted message is ${data.toString}")
      println(s"Secret message is ${textEncryptor.decrypt(data.toString)}")
      sender() ! SendToConnection(ByteString.fromString("server reply\r\n"))
    case err@_ => failure(err)
  }

  def failure(msg: Any) = msg match {
    case ProtocolEnded(err) =>
      // Protocol has been ended. End self then tell parent
      println(err)
      sender() ! ChildFinished

    case unknown@_ => println(s"unknown message: $unknown")
  }
}