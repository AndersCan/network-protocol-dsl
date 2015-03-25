package com.protocoldsl.crypto

import scala.annotation.tailrec

/**
 * Created by aoc4 on 25/03/15.
 */
object Helper {
  val ZERO = BigInt(0)

  def pow(x: BigInt, y: BigInt): BigInt = {
    //println(s"mul: $mul and y: $y")
    @tailrec
    def loop(n: BigInt, acc: BigInt): BigInt = {
      if (n == ZERO) acc
      else loop(n - 1, x * acc)
    }
    loop(y, 1)
  }

  /**
   * Test whether given value is holds true to Fermats Little Theorem, if true: p is declared probably prime. IE. Not guaranteed to be correct.
   * Range of a is 1 to 10000
   */
  def fermat(p: BigInt): Boolean = {
    val a = BigInt(1 + (scala.math.random * 10000).toInt)
    (pow(a, p) - a) % p == ZERO
  }

}
