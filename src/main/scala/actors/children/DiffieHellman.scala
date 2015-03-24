package actors.children

import java.lang.Integer

import actors.{ToProtocolMaster, ToChildMessage}
import akka.actor.{Props, Actor}
import akka.actor.Actor.Receive
import akka.util.ByteString

/**
 * Created by aoc4 on 23/03/15.
 */

object DiffieHellman {
  def props() = Props(classOf[DiffieHellman])
}

class DiffieHellman extends Actor {
  val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter(i => primes.takeWhile(j => j * j <= i).forall(k => i % k > 0))
  //  primes.take(2)

  var prime = 0.0
  val privateKey = 10.0
  val generator = 2.0
  var myPublicKey = 0.0

  var sharedSecret = 0.0 // only A and B knows this


  var currentStep = "prime"

  override def receive: Receive = {
    case ToChildMessage(data) => {
      println(" CurrentSTEP YO: " + currentStep)
      currentStep match {
        case "prime" =>
          println("Current step: " + currentStep)
          val receivedValue = data.utf8String.dropRight(2).toDouble
          prime = receivedValue
          println(s"($generator ^ $privateKey) % $prime")
          myPublicKey = scala.math.pow(generator, privateKey) % prime
          sender() ! ToProtocolMaster(ByteString.fromString(myPublicKey + "\r\n"))
          println(s"myPubK: $myPublicKey")
          currentStep = "gotpubkey"
        case "gotpubkey" =>
          val receivedValue = data.utf8String.dropRight(2).toDouble
          sharedSecret = scala.math.pow(receivedValue, privateKey) % prime
          println(s"($receivedValue ^ $privateKey) % $prime")
          // sender() ! ToProtocolMaster(ByteString.fromString(myPublicKey + "\r\n"))
          currentStep = "secured"
        case "secured" => println(s"Shared secret is ${sharedSecret}")
        case _ => println(s"We got: ${data.utf8String.dropRight(2)}")
      }
    }
    case _ => println("unknown message")

  }
}
