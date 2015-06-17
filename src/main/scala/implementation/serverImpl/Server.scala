package implementation.serverImpl

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import com.protocoldsl.actors.ProtocolMonitor
import com.protocoldsl.protocol.{Branch, ProtocolBuilder, ValidationError, Validator}
import implementation.clientImpl.{PubKey, Username}
import implementation.crypto.Helper
import implementation.serverImpl.children._
import implementation.serverImpl.performance.EchoMessage
import implementation.protocols.ProtocolCollection
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

  def receive = {
    case b@Bound(localAddress) =>
      // do some logging or setup ...
      println("Server bond to: " + localAddress)
    case CommandFailed(_: Bind) => context stop self

    case cu@Connected(remote, local) =>
      //println(s"New Connection: remote: $remote, local: $local")
      // --- Server Protocols ---

      //      val proto = ProtocolCollection.EchoServerProtocol.compile
      //      val proto = ProtocolCollection.MultiplyServerProtocol.compile
      //      val proto = ProtocolCollection.MulorEchoServerProtocol.compile
      val proto = ProtocolCollection.SecureChatProtocol.compile
      //      val proto = ProtocolCollection.HTTPServerProtocol.compile


      //      val consumer = context.actorOf(MulOrEcho.props())
      val consumer = context.actorOf(SecureChatServer.props())
      //      val consumer = context.actorOf(HTTPServer.props())


      // Sender() is sender of the current message
      val connection = sender()
      val handler = context.actorOf(ProtocolMonitor.props(proto, connection, consumer))
      connection ! Register(handler)
  }

  override def preStart(): Unit = {
    IO(Tcp) ! Bind(self, inetSocketAddress)
  }

}
