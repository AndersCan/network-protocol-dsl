package implementation.serverImpl.children

import akka.actor.{Actor, PoisonPill, Props}
import com.protocoldsl.actors.{ChildFinished, Initiation, ProtocolEnded, ToConnection}

/**
 * Created by aoc4 on 07/06/15.
 */

sealed abstract class HttpField

case class RequestType(method: String, resource: String, httpVersion: String) extends HttpField

case class HttpHost(host: String) extends HttpField

case class HttpAccept(accept: String) extends HttpField

case class UserAgent(agent: String) extends HttpField

case class HTTPRequest(headers: List[HttpField])


object HTTPServer {
  def props() = Props(classOf[HTTPServer])
}

class HTTPServer extends Actor {
  def receive = {
    case HTTPRequest(headers) =>
//      implicit val iRequest = req
      if (headers.isEmpty) {
        // TODO send response
//
      } else {
        headers.head match {
          case RequestType(method, resource, httpVersion) =>
            resource match {
              case "/" =>
                sender() ! ToConnection(s"$headers<html> <body> <h1> Index page </h1> getRequestInfo </body> </html> ")
              case "/test1" =>
                sender() ! ToConnection(s"$headers<html> <body> <h1> Test 1 Page </h1> getRequestInfo </body> </html> ")
              case "/test2" =>
                sender() ! ToConnection(s"$headers<html> <body> <h1> Test 2 Page </h1> getRequestInfo </body> </html> ")
              case request@_ =>
                sender() ! ToConnection(s"HTTP/1.1 404 NOT FOUND\r\nContent-Type:text/html;\r\n\n<html> <body> <h1> 404 NOT FOUND. " +
                  s"<h2>Valid are: /, /test1, /test2</h2> </h1> getRequestInfo </body> </html> ")
                println(s"We don't serve: $request")
            }
        }
      }
      sender() ! ChildFinished
      self ! PoisonPill
    case Initiation =>
      println("New connection")
    case ProtocolEnded(reason) =>
      println(reason)
      println("Stopping HTTP Server")
      sender() ! ChildFinished
      self ! PoisonPill
    case err@_ =>
      println(s"HTTP Server error: $err")
      sender() ! ChildFinished
      self ! PoisonPill
  }

  def headers: String = {
    "HTTP/1.1 200 OK\r\nContent-Type:text/html;\r\n\n"
  }

//  def getRequestInfo(implicit httpRequest: HTTPRequest): String = {
//    s"<h2>You asked for resource: ${httpRequest.requestType.resource}</h2>" +
//      s"<h4>Your Agent is specified as: ${httpRequest.userAgent.agent}</h4>"
//  }

  override def postStop(): Unit = {
    println("HTTPS Server stopping...")
  }
}
