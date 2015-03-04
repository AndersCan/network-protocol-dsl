import java.net.InetSocketAddress

import actors.Server
import akka.actor.ActorSystem

import scala.io.StdIn

/**
 * Created by anders on 04/03/15.
 */
object Main extends App {

  val system = ActorSystem("echo-service-system")
  val endpoint = new InetSocketAddress("localhost", 11111)
  system.actorOf(Server.props(endpoint), "echo-service")

  scala.io.StdIn.readLine(s"Hit ENTER to exit ...${System.getProperty("line.separator")}")
  system.shutdown()
}