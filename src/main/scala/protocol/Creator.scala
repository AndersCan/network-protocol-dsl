package protocol

/**
 * Created by anders on 04/03/15.
 */
case class Validator(f: String => Either[String, Boolean])

object Creator {

  val p = Creator.generateProtocol((x: Int) => {
    "x"
  })

  println(p)

  def generateProtocol(intToString: (Int) => String) = {
    5
  }

  val c = new Socket()
  val s = new Socket()

  val isString = new Validator(x => Right(true))

  val isInt = new Validator(x => try {
    Integer.parseInt(x)
    Right(true)
  } catch {
    case e: Exception => Left("msg breaks protocol. not int")
  })

  // todo Create function instead of String compares
  c send(s, isInt)
  s send(c, isInt)

  println(c.states)
  println(s.states)
}