package protocol

import com.protocoldsl.protocol._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by aoc4 on 07/04/15.
 */

class ProtocolBuilderTests extends FlatSpec with Matchers {
  val p = ProtocolBuilder()
  val isAnything = new Validator(_ => Right(true))
  "The ProtocolBuilder" must "be able to add states" in {
    p sends isAnything
  }
  it must "be immutable" in {
    p sends isAnything receives isAnything loop() branchOn _
    assert(p.compile.protocolStates.length == 0)
  }
  it must "be able to compile Protocol from ProtocolBuilder" in {
    val states: Protocol = (p sends isAnything).compile
  }
  it must "contain correct order of states" in {
    val states: Protocol = (p sends isAnything receives isAnything loop()).compile
    assert(states.protocolStates.head.isInstanceOf[Send])
    assert(states.protocolStates.tail.head.isInstanceOf[Receive])
    assert(states.protocolStates.tail.tail.head.isInstanceOf[Loop])
  }
  it must "make next and looped look similar " in {
    val sugared = p sends isAnything next (p anyone isAnything loop())
    val unsugared = p sends isAnything looped(0, p anyone isAnything loop())
    assert(sugared.compile.toString() == unsugared.compile.toString())
  }
  it must "make next and looped NOT look similar" in {
    val sugared = p sends isAnything sends isAnything next (p anyone isAnything loop())
    val unsugared = p sends isAnything looped(0, p anyone isAnything loop())
    assert(!(sugared.compile.toString() == unsugared.compile.toString()))
  }
}
