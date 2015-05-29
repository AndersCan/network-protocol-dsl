package implementation.clientImpl

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import implementation.clientImpl.performance.EchoClient

/**
 * Created by anders on 17/04/15.
 */
object ClientMain extends App {

  val system = ActorSystem("diffie-service-system")
  val remoteHost = new InetSocketAddress("localhost", 8888)
  system.actorOf(EchoClient.props(remoteHost), "diffie-service")

  scala.io.StdIn.readLine(s"Hit ENTER to exit ...${System.getProperty("line.separator")}")
  system.shutdown()
}
