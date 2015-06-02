import implementation.crypto.Helper
import org.scalatest.{FlatSpec, Matchers}


/**
 * Created by aoc4 on 25/03/15.
 */

class CryptoTests extends FlatSpec with Matchers {
  "The Crypto Helper" should "correctly identify first 20 prime numbers" in {
    //    val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter(i => primes.takeWhile(j => j * j <= i).forall(k => i % k > 0))
    //    primes take 10 forall (p => Helper.fermat(p))
    val ps = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71)
    for (p <- ps) {
      assert(Helper.fermat(p))
    }
  }

  it should "calculate the power of BigInts" in {
    //    for (a <- 1 to 10) {
    var i = 0
    while (i < 100) {
      val x: Int = (scala.math.random * 10).toInt
      val y: Int = (scala.math.random * 10).toInt
      val smallPower = scala.math.pow(x, y).toInt
      val bigPower = Helper.pow(BigInt(x), BigInt(y))
      assert(bigPower == BigInt(smallPower))
      i += 1
    }
  }

}
