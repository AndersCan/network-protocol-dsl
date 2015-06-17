package implementation.protocols

import com.protocoldsl.protocol.{ProtocolBuilder, ValidationError, Validator, Branch}
import implementation.clientImpl.{Username, PubKey}
import implementation.crypto.Helper
import implementation.serverImpl.children.{AnInteger, PrimeAndGenerator, EncryptedChatMessage}
import net.liftweb.json._

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

  val HTTPServerProtocol = endpoint receives HeaderRequest sends anything
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

  // --=Echo Server=--

  var previous = 0
  private val anInteger = new Validator(input => try {
//    println(s"Previous validated message was: $previous")
    if (input.contains("\n")) {
      previous = input.dropRight(2).toInt
      Right(AnInteger(input.dropRight(2).toInt))
    } else {
      Right(input)
    }
  } catch {
    case e: Exception =>
      Left(ValidationError(s"${input.dropRight(2)} is not anInteger", e))
  })


  // Echo Server
  val EchoServerProtocol = endpoint receives anything sends anything loop()

  // Multiply Server
  val MultiplyServerProtocol = endpoint receives anInteger receives anInteger sends anInteger loop()

  // Multiply or EchoServerProtoco
  private val mulEchoBranch = Branch(input => {
    val maybeNumber = if (input.contains("\n")) input.dropRight(2) else input
    if (maybeNumber forall Character.isDigit) MultiplyServerProtocol
    else EchoServerProtocol
  })

  val MulorEchoServerProtocol = endpoint branchOn mulEchoBranch


  // --=Secure Chat Server=--

  private implicit val formats = DefaultFormats
  private val primeAndGenerator = new Validator(input => try {
    val json = parse(input)
    val p = (json \ "prime").extract[String].toDouble
    val g = json \ "generator"
    if (Helper.fermat(p)) {
      println("OKAY....")
      Right(PrimeAndGenerator(prime = p, generator = g.extract[String].toDouble))
    } else {
      Left(ValidationError(s"PrimeAndGenerator: $p is not a Prime number"))
    }
  } catch {
    case e: Exception =>
      Left(ValidationError("Msg could not be converted to a " +
        "PrimeAndGenerator class: ", e))
  })
  private val aPublicKey = new Validator(input => try {
    val json = parse(input)
    val pk = json \ "publickey"
    // TODO FIX, PubKey class
    Right(PubKey(pk.extract[String].toDouble))
  } catch {
    case e: Exception =>
      Left(ValidationError("Msg could not be converted to a PrimeAndGenerator class: ", e))
  })

  private val aUsername = new Validator(input => try {
    Right(Username(input))
  } catch {
    case e: Exception =>
      Left(ValidationError("msg breaks protocol. not a username", e))
  })

  private val aChatMessage = new Validator(input => try {
    Right(EncryptedChatMessage(input))
  } catch {
    case e: Exception =>
      Left(ValidationError("Message was not a chatmessage", e))
  })

  //Diffie Initiation
  private val diffieInit = endpoint receives primeAndGenerator sends aPublicKey receives aPublicKey
  val SecureChatProtocol = diffieInit receives aUsername next (endpoint anyone aChatMessage loop())


}
