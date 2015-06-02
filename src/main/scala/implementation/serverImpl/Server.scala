package implementation.serverImpl

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.ProtocolMonitor
import com.protocoldsl.protocol.{ProtocolBuilder, ValidationError, Validator}
import implementation.serverImpl.children.PrimeAndGenerator
import implementation.serverImpl.performance.{EchoMessage, EchoServerActor}
import net.liftweb.json._
/**
 * Created by anders on 04/03/15.
 */

object Server {
  def props(inetSocketAddress: InetSocketAddress) = Props(classOf[Server], inetSocketAddress)
}

class Server(inetSocketAddress: InetSocketAddress) extends Actor {

  import akka.io.Tcp._
  import context.system

  val isAnything = new Validator(in => Right(EchoMessage(in)))
  //  val nothing = new Validator(_ => Left("Nothing will always give a Left()"))
  //
  //  val isInt = new Validator(x => try {
  //    Right(IsInt(x.dropRight(2).toInt))
  //  } catch {
  //    case e: Exception =>
  //      Left("msg breaks protocol. not int")
  //  })
  //
  val aUsername = new Validator(x => try {
    if (x.contains("\n")) {
      Right(x.dropRight(2))
    } else {
      Right(x)
    }
  } catch {
    case e: Exception =>
      Left(ValidationError("msg breaks protocol. not a username", e))
  })

  val isDouble = new Validator(x => try {
    if (x.contains("\n")) {
      Right(x.dropRight(2).toDouble)
    } else {
      Right(x.toDouble)
    }
  } catch {
    case e: Exception =>
      Left(ValidationError("msg breaks protocol. not double", e))
  })

  val isPrime = new Validator(input => try {
    val maybePrime: BigInt = BigInt(input.toString.dropRight(2))
    val result = com.protocoldsl.crypto.Helper.fermat(maybePrime)
    if (result) Right(input.toDouble)
    else Left(ValidationError("Not prime"))
  } catch {
    case e: Exception =>
      Left(ValidationError("msg breaks protocol. not Prime", e))
  })

  implicit val formats = DefaultFormats
  val primeAndGenerator = new Validator(input => try {
    val json = parse(input)
    val p = json \ "prime"
    val g = json \ "generator"
    Right(PrimeAndGenerator(prime = p.extract[Double], generator = g.extract[Double]))
  } catch {
    case e: Exception =>
      Left(ValidationError("Msg could not be converted to a " +
        "PrimeAndGenerator class: ", e))
  })

  val aPublicKey = new Validator(input => try {
    val json = parse(input)
    val p = json \ "prime"
    val g = json \ "generator"
    // TODO FIX, PubKey class
    Right(PrimeAndGenerator(prime = p.extract[Double], generator = g.extract[Double]))
  } catch {
    case e: Exception =>
      Left(ValidationError("Msg could not be converted to a PrimeAndGenerator class: ", e))
  })

  // TODO - How to add multiple communication channels? {S :: C :: C}
  val server = new ProtocolBuilder()

  //  val mulServer =
  //    server receives isInt receives isInt sends isAnything loop()

  val echoServer = ProtocolBuilder.loop(
    server receives isAnything sends isAnything
  )
  /*
    val mulOrEchoTest = new Branch(input =>
      if (input forall Character.isDigit) mulServer
      else echoServer
    )
  */
  //val branching = ProtocolBuilder() branchOn mulOrEchoTest

  //Diffie Initiation
  val diffieInit = server receives isPrime sends isDouble receives isDouble
  //Diffie Server Chat
  //val diffieProtocol = diffieInit looped(0, server anyone isAnything loop())

  //SecureChat
  //  val securechatProtocol = diffieInit receive username send usernames receive username next server anyone isAnything loop()

  val securechatProtocol = diffieInit receives aUsername looped(0, server anyone isAnything loop())


  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)

    case CommandFailed(_: Bind) => context stop self

    case cu@Connected(remote, local) =>
      println(s"New Connection: remote: $remote, local: $local")
      val proto = echoServer.compile
      val consumer = context.actorOf(EchoServerActor.props())
      // Sender() is sender of the current message
      val connection = sender()
      val handler = context.actorOf(ProtocolMonitor.props(proto, connection, consumer))
      connection ! Register(handler)
  }

  override def preStart(): Unit = {
    IO(Tcp) ! Bind(self, inetSocketAddress)
  }

}