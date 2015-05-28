package protocol

import com.protocoldsl.protocol._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by aoc4 on 07/04/15.
 */

class ProtocolBuilderTests extends FlatSpec with Matchers {
  val isAnything = new Validator(_ => Right(true))
  "The ProtocolBuilder" must "be able to add states" in {
    val p = new ProtocolBuilder()
    p sends isAnything
  }
  it must "be immutable" in {
    val p1 = new ProtocolBuilder()
    p1 sends isAnything receives isAnything loop() branchOn _
    assert(p1.compile.protocolStates.length == 0)
  }
  it must "be able to compile Protocol" in {
    val p = new ProtocolBuilder()
    val states: Protocol = (p sends isAnything).compile
  }
  it must "must contain correct order of states" in {
    val p = new ProtocolBuilder()
    val states: Protocol = (p sends isAnything receives isAnything loop()).compile
    assert(states.protocolStates.head.isInstanceOf[Send])
    assert(states.protocolStates.tail.head.isInstanceOf[Receive])
    assert(states.protocolStates.tail.tail.head.isInstanceOf[Loop])
  }


}
