package implementation.actors

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.ProtocolMaster
import com.protocoldsl.protocol.{ProtocolBuilder, Validator}
import implementation.actors.children.SimplisticHandler

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
  val nothing = new Validator(_ => Left("NO - Run"))

  val isInt = new Validator(x => try {
    // Remove \n from end of line
    x.dropRight(2).toInt
    Right(true)
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })

  val intGT5 = new Validator(x => try {
    // Remove \n from end of line
    val value = x.dropRight(2).toInt
    if (value > 5) Right(true)
    else Left("Not Greater than 5")
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
      Left("msg breaks protocol. not double")
  })


  val isPrime = new Validator(input => try {
    // Remove \n from end of line
    val maybePrime: BigInt = BigInt(input.dropRight(2).toString)
    println(maybePrime)
    val result = com.protocoldsl.crypto.Helper.fermat(maybePrime)
    println(result)
    if (result) Right(true)
    else Left("Not prime")
  } catch {
    case e: Exception =>
      Left("msg breaks protocol. not int")
  })


  // TODO - How to add multiple communication channels? {S :: C :: C}
  val client = new ProtocolBuilder()
  val server = new ProtocolBuilder()
  val server2 = new ProtocolBuilder()

  // Multiply SERVER
  //  c send(s, isInt)
  //  c send(s, isInt)
  //  s send(c, isAnything) // String
  //  s gotoStep 0

  client send isInt send isInt receive isAnything
  //  server receive isInt receive isInt send isAnything

  val serverloop = ProtocolBuilder.loop(
    server receive isInt receive isInt send isAnything
  )
  //  val hehe = ProtocolBuilder.loop(
  //    server2 receive isDouble receive isDouble send isAnything
  //  )
  //
  //
  //  val branching: ProtocolBuilder = new ProtocolBuilder().branch(serverloop, intGT5, hehe)

  // END multiply SERVER

  //Diffie

  //    c send(s, isPrime) // sends prime
  //    s send(c, isDouble) // sends shared secret
  //    c send(s, isDouble) // sends shared secret
  //    c send(s, isAnything)
  //    s send(c, isAnything)
  //    s gotoStep 3

  // loop (boolean, f(true),f(false)
  // function pb methods that take functions that compute a new protocol builder)
  //loop(
  // new pbuilder
  // c send (s, type)
  // )

  //  val proto = c.compile
  //  val proto = s.compile

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)

    case CommandFailed(_: Bind) => context stop self

    case cu@Connected(remote, local) =>
      println(s"New Connection: remote: $remote, local: $local")
      val proto = serverloop.compile
      //      val diffie = context.actorOf(DiffieHellman.props())
      val child = context.actorOf(SimplisticHandler.props())
      // Sender() is sender of the current message
      val connection = sender()
      val handler = context.actorOf(ProtocolMaster.props(proto, connection, child))
      //      val handler = context.actorOf(Props[SimplisticHandler])
      connection ! Register(handler)
  }

}