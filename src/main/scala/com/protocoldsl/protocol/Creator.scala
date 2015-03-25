//package protocol
//
///**
// * Created by anders on 04/03/15.
// */
//
//
//object Creator {
//
//  val c = new Socket()
//  val s = new Socket()
//
//  val isString = new Validator(x => Right(true))
//
//  val isInt = new Validator(x => try {
//    Integer.parseInt(x)
//    Right(true)
//  } catch {
//    case e: Exception => Left("msg breaks protocol. not int")
//  })
//
//  // todo Create function instead of String compares
//  c send(s, isInt)
//  s send(c, isInt)
//
//  println(c.states)
//  println(s.states)
//}