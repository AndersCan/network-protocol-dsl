package implementation.clientImpl.util

import akka.actor.{Props, Actor}
import akka.util.ByteString
import com.protocoldsl.actors.SendToConnection
import implementation.clientImpl.ChatMessage
import org.jasypt.util.text.BasicTextEncryptor

/**
 * Created by anders on 09/05/15.
 */

case class StartDiffie()

case class Prime(p: Double)

case class PublicKey(pk: Double)

object DiffieHellman {
  def props(ourUsername: String, theirUsername: String) =
    Props(classOf[DiffieHellman], ourUsername, theirUsername)
}

class DiffieHellman(ourUsername: String, theirUsername: String) extends Actor {

  val GENERATOR = 2.0
  var prime2 = BigInt.probablePrime(8, scala.util.Random).toDouble
  val privateKey2 = math.abs(scala.util.Random.nextInt(10) + 1)
  var myPublicKey2 = 0.0
  // only A and C knows this
  var sharedSecret2 = 0.0
  var chatsecure = false
  var user1 = false
  val chatEncryptor = new BasicTextEncryptor()


  def receive: Receive = {
    case StartDiffie() =>
      println(s"Starting DiffieHellman with user: $theirUsername")
      // Start DiffieInit
      val diffieInit = s""" { "token" : "SecureInit", "header" : "Prime", "from" : "$ourUsername", "to" : "$theirUsername", "number" : $prime2 } """
      sender() ! SendToConnection(bs(diffieInit))
      myPublicKey2 = scala.math.pow(GENERATOR, privateKey2) % prime2
    case Prime(p) =>
      prime2 = p
      myPublicKey2 = scala.math.pow(GENERATOR, privateKey2) % prime2
      println(s"Sending myPublickey: $myPublicKey2")
      val jsonpubkey = s"""{ "token" : "SecureInit", "header" : "PubKey", "from" : "$ourUsername", "to" : "$theirUsername",  "number" : "$myPublicKey2" }"""
      sender() ! SendToConnection(bs(jsonpubkey))
    case PublicKey(pk) =>
      if (!chatsecure) {
        println(s"Received public key: $pk")
        chatsecure = true
        sharedSecret2 = scala.math.pow(pk, privateKey2) % prime2
        println(s"Shared Secret2: ($pk^$privateKey2) % $prime2")
        println(s"Shared ChatSecret: ${sharedSecret2.toString}")
        chatEncryptor.setPassword(sharedSecret2.toString)
        val jsonpubkey = s"""{ "token" : "SecureInit", "header" : "PubKey", "from" : "$ourUsername", "to" : $theirUsername, "number" : "$myPublicKey2" }"""
        sender() ! SendToConnection(bs(jsonpubkey))
      } else {
        println("Sending message...")
        val jsonchatmsg = s"""{ "token" : "ChatMessage", "from" : "$ourUsername", "to" : $theirUsername, "msg" : "${chatEncryptor.encrypt("Hello my dear friend")}" }"""
        sender() ! SendToConnection(bs(jsonchatmsg))
      }
    case ChatMessage(from, to, msg) =>
      println(s"${chatEncryptor.decrypt(msg)}, sent from: $from")
    case err@_ =>
      println(s"Error: unknown message: $err")
  }


  // encrypt and create bytestring
  def sec(in: String): ByteString = {
    bs(chatEncryptor.encrypt(in + ""))
  }

  // create bytestring
  def bs(in: String): ByteString = {
    ByteString.fromString(in)
  }
}
