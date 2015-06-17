package implementation.crypto

import scala.annotation.tailrec

/**
 * Created by aoc4 on 25/03/15.
 */
object Helper {
  val ZERO = BigInt(0)
  // TODO Add implicits for allowing "overloaded" functions
  /**
   * Doesn't work for large exponents, function will take too long.
   * @param x number to be taken to the power of y
   * @param y exponent of x
   * @return x to the power of y
   */
  def pow(x: BigInt, y: BigInt): BigInt = {
    @tailrec
    def loop(n: BigInt, acc: BigInt): BigInt = {
      if (n == ZERO) acc
      else loop(n - 1, x * acc)
    }
    loop(y, 1)
  }

  //
  //  def pow(x: Double, y: Double): Double = {
  //    @tailrec
  //    def loop(n: Double, acc: Double): Double = {
  //      if (n == 0) acc
  //      else loop(n - 1, x * acc)
  //    }
  //    loop(y, 1)
  //  }

  /**
   * Test whether given value is holds true to Fermats Little Theorem, if true: p is declared probably prime. IE. Not guaranteed to be correct.
   * Range of a is 1 to 10
   */
  //  def fermat(p: BigInt, repeats: Int = 100): Boolean = {
  //    var i = 0
  //    while (i < repeats) {
  //      val a = BigInt(1 + (scala.math.random * 10).toInt)
  //      if ((pow(a, p) - a) % p != ZERO) return false
  //      i = i + 1
  //    }
  //    true
  //  }

  def fermat(p: Double, repeats: Int = 100): Boolean = {
    val prime = BigInt(p.toInt.toString)
    var i = 0
    while (i < repeats) {
      val a = BigInt(1 + (scala.math.random * 10).toInt)
      if ((pow(a, prime) - a) % prime != ZERO) return false
      i = i + 1
    }
    true
  }

}
