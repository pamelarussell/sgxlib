package feature

import org.scalatest.FunSuite
import Orientation._

/**
  * Created by prussell on 8/30/16.
  */
class OrientationSuite extends FunSuite {

  test("Orientation equality") {
    assert(Plus === Plus)
    assert(Minus === Minus)
    assert(Both === Both)
    assert(Unstranded === Unstranded)
  }

  test("Consensus orientation") {
    assert(consensus(Plus, Plus) === Plus)
    assert(consensus(Plus, Minus) === Unstranded)
    assert(consensus(Plus, Both) === Plus)
    assert(consensus(Plus, Unstranded) === Unstranded)
    assert(consensus(Minus, Plus) === Unstranded)
    assert(consensus(Minus, Minus) === Minus)
    assert(consensus(Minus, Both) === Minus)
    assert(consensus(Minus, Unstranded) === Unstranded)
    assert(consensus(Both, Plus) === Plus)
    assert(consensus(Both, Minus) === Minus)
    assert(consensus(Both, Both) === Both)
    assert(consensus(Both, Unstranded) === Unstranded)
    assert(consensus(Unstranded, Plus) === Unstranded)
    assert(consensus(Unstranded, Minus) === Unstranded)
    assert(consensus(Unstranded, Both) === Unstranded)
    assert(consensus(Unstranded, Unstranded) === Unstranded)
  }

  test("Can overlap") {
    assert(isCompatible(Plus, Plus))
    assert(!isCompatible(Plus, Minus))
    assert(isCompatible(Plus, Both))
    assert(!isCompatible(Plus, Unstranded))
    assert(!isCompatible(Minus, Plus))
    assert(isCompatible(Minus, Minus))
    assert(isCompatible(Minus, Both))
    assert(!isCompatible(Minus, Unstranded))
    assert(isCompatible(Both, Plus))
    assert(isCompatible(Both, Minus))
    assert(isCompatible(Both, Both))
    assert(!isCompatible(Both, Unstranded))
    assert(!isCompatible(Unstranded, Plus))
    assert(!isCompatible(Unstranded, Minus))
    assert(!isCompatible(Unstranded, Both))
    assert(isCompatible(Unstranded, Unstranded))
  }

}