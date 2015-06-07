package implementation.protocols

import com.protocoldsl.protocol.{ProtocolBuilder, ValidationError, Validator}

/**
 * Created by aoc4 on 07/06/15.
 */

case class RequestType(method: String, resource: String, httpVersion: String)

case class HttpHost(host: String)

case class HttpAccept(accept: String)

case class UserAgent(agent: String)

case class HTTPRequest(requestType: RequestType, httpHost: HttpHost, httpAccept: HttpAccept, userAgent: UserAgent)

object ProtocolCollection {
  private val endpoint = ProtocolBuilder()
  private val anything = new Validator(in => Right(in))

  // --=HTTP Validators=--
  // This validator only gets the first line as the RequestType (does not handle unordered headers)
  private val HeaderRequest = new Validator(input => try {
    // This Must work for the request to be valid
    val headerLines: Array[String] = input.split("\r\n")
    val requestLines = headerLines(0).split(" ")
    val requestType = RequestType(method = requestLines(0), resource = requestLines(1), httpVersion = requestLines(2))
    val rawMap = scala.collection.mutable.Map[String, String]()

    for (i <- 1 until headerLines.length) {
      if (headerLines(i).contains(": ")) {
        val details = headerLines(i).split(": ")
        rawMap += details(0) -> details(1)
      }
    }

    val httpHost = HttpHost(rawMap.getOrElse("Host", ""))
    val httpAccept = HttpAccept(rawMap.getOrElse("Accept", ""))
    val userAgent = UserAgent(rawMap.getOrElse("User-Agent", ""))

    Right(HTTPRequest(requestType: RequestType, httpHost: HttpHost, httpAccept: HttpAccept, userAgent: UserAgent))

  } catch {
    case e: Exception =>
      Left(ValidationError("Could not get HeaderRequest", e))
  })

  val HTTPServer = endpoint receives HeaderRequest sends anything
  /*
  GET / HTTP/1.1
  Host: localhost:8888
  Connection: keep-alive
  Cache-Control: max-age=0
  Accept: text/html
  User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.81 Safari/537.36
  DNT: 1
  Accept-Encoding: gzip, deflate, sdch
  Accept-Language: en-US,en;q=0.8,no;q=0.6
   */

  // END --=HTTP Validators=--

}
