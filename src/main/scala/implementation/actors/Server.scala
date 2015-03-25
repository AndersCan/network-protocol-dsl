package implementation.actors

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.ProtocolMaster
import com.protocoldsl.protocol.{ProtocolBuilder, Validator}
import implementation.actors.children.DiffieHellman

/**
 * Created by anders on 04/03/15.
 */

object Server {
  def props(inetSocketAddress: InetSocketAddress) = Props(classOf[Server], inetSocketAddress)
}

class Server(inetSocketAddress: InetSocketAddress) extends Actor {

  import akka.io.Tcp._
  import context.system

  //  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 0))
  IO(Tcp) ! Bind(self, inetSocketAddress)

  val isAnything = new Validator(_ => Right(true))
  val isInt = new Validator(x => try {
    // Remove \n from end of line
    x.dropRight(2).toInt
    Right(true)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })
  val isDouble = new Validator(x => try {
    // Remove \n from end of line
    x.dropRight(2).toDouble
    Right(true)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })

  // http://www.codecodex.com/wiki/Calculate_an_integer_square_root
  //  def sqrt(number : BigInt) = {
  //    def next(n : BigInt, i : BigInt) : BigInt = (n + i/n) >> 1
  //    val one = BigInt(1)
  //    var n = one
  //    var n1 = next(n, number)
  //    while ((n1 - n).abs > one) {
  //      n = n1
  //      n1 = next(n, number)
  //    }
  //    while (n1 * n1 > number) {
  //      n1 -= one
  //    }
  //    n1
  //  }


  val isPrime = new Validator(input => {
    //    try {
    // Remove \n from end of line
    val maybePrime: BigInt = BigInt(input.dropRight(2).toString)
    //    val maybePrime: BigInt = BigInt(input.dropRight(2).toString.map(_.asDigit).map(BigInt(_)).toString())
    //    val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter(i => primes.takeWhile(j => j * j <= i).forall(k => i % k > 0))

    println(maybePrime)
    val result = com.protocoldsl.crypto.Helper.fermat(maybePrime)
    println(result)
    if (result) Right(true)
    else Left("Not prime")
    //    val result1 = Future {
    //      fermat(maybePrime)
    //    }
    //    val result2 = Future {
    //      fermat(maybePrime)
    //    }
    //    val result3 = Future {
    //      fermat(maybePrime)
    //    }
    //
    //    val veryLikelyPrime: Future[Either[String,Boolean]] = Future {
    //      for {
    //        r1 <- result1
    //        r2 <- result2
    //        r3 <- result3
    //      } yield r1 && r2 && r3
    //    }
    //    veryLikelyPrime
    //    val result = Await.result(veryLikelyPrime, 10 seconds)
    //    veryLikelyPrime.onSuccess(Right(true))
    //    veryLikelyPrime.onFailure(Left(""))
    //    result2.value
    //    result match {
    //      case Success(value) => Right(true)
    //      case Failure(e) => Left("msg breaks protocol. not prime")
    //      case _ => Left("unknown error when calculating prime")
    //    }
  }
    //      Right(true)
    //    } catch {
    //      case e: Exception =>
    //        Left("msg breaks protocol. not int")
    //    }
  )


  // TODO - How to add multiple communication channels? {S :: C :: C}
  val c = new ProtocolBuilder()
  val s = new ProtocolBuilder()

  // ECHO SERVER
  //  c send(s, isInt)
  //  c send(s, isInt)
  //  s send(c, new Validator(_ => Right(true))) // String
  //  c gotoStep 0
  // END ECHO SERVER

  //Diffie

  c send(s, isPrime) // sends prime
  s send(c, isDouble) // sends shared secret
  c send(s, isDouble) // sends shared secret
  c send(s, isAnything)
  s send(c, isAnything)
  c gotoStep 3


  //  val proto = c.compile
  //  val proto = s.compile

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)

    case CommandFailed(_: Bind) => context stop self

    case c@Connected(remote, local) =>
      println(s"New Connection: remote: $remote, local: $local")
      val proto = s.compile
      val diffie = context.actorOf(DiffieHellman.props())
      //      val child = context.actorOf(SimplisticHandler.props())
      // Sender() is sender of the current message
      val connection = sender()
      val handler = context.actorOf(ProtocolMaster.props(proto, connection, diffie))
      //      val handler = context.actorOf(Props[SimplisticHandler])
      connection ! Register(handler)
  }

}