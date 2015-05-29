package implementation.clientImpl.performance

/**
 * Created by aoc4 on 28/05/15.
 *
 * Actor that measures the time it takes to send and receive 1,000,000 messages from a server.
 */

import java.net.InetSocketAddress
import java.util.Calendar

import akka.actor._
import akka.io.{IO, Tcp}
import akka.util.ByteString

/**
 * Created by anders on 04/03/15.
 */

object EchoClient {
  def props(remoteHost: InetSocketAddress) = Props(classOf[EchoClient], remoteHost)
}

class EchoClient(remoteHost: InetSocketAddress) extends Actor {
  val totalmessages = 100000

  var NOW: Calendar = null
  var DONE: Calendar = null
  var counter = 0

  var connection: ActorRef = null

  import akka.io.Tcp._
  import context.system

  def receive = {

    case CommandFailed(_: Bind) => context stop self
    case cu@Connected(remote, local) =>
      println(s"New Connection: remote: $remote, local: $local")
      // Sender() is sender of the current message
      connection = sender()
      connection ! Register(self)
      self ! "START"
    case Received(data) =>
      // We got data
      counter += 1

    // Test if we have received 1 000000 msgs
          if (counter == totalmessages) {
            DONE = Calendar.getInstance()
            val result = DONE.getTimeInMillis - NOW.getTimeInMillis
            println("Done: " + result)
            println("ServerMessage of Done: " + data.utf8String)
            // RESTART
            counter = 0
            NOW = Calendar.getInstance()
          }
      connection ! Write(ByteString.fromString("Message: " + counter))
    case "START" =>
      counter = 0
      NOW = Calendar.getInstance()
      connection ! Write(ByteString.fromString("MESSAGE"))
    case err@_ =>
      println(s"Sending: $counter")
      println("Messages before fail: " + counter)
      println("ClientServer got an unexpected message: " + err)
  }

  override def preStart(): Unit = {
    IO(Tcp) ! Connect(remoteHost)
    //
  }
}
