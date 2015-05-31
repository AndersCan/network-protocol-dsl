package implementation.serverImpl

import java.net.InetSocketAddress

import akka.actor.ActorSystem


/**
 * Created by anders on 04/03/15.
 */
object ServerMain extends App {

  val system = ActorSystem("echo-service-system")
  val endpoint = new InetSocketAddress("localhost", 8888)
  // No PM
//    system.actorOf(EchoServer.props(endpoint), "echo-service")
  // With PM
  system.actorOf(Server.props(endpoint), "echo-service")

  scala.io.StdIn.readLine(s"Hit ENTER to exit ...${System.getProperty("line.separator")}")
  system.shutdown()
}