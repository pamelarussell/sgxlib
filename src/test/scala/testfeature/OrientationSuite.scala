package testfeature

import feature.{Unstranded, _}
import feature.Orientation._
import org.scalatest.FunSuite

/**
  * Created by prussell on 8/30/16.
  */
class OrientationSuite extends FunSuite {

  test("Orientation equality") {
    assert(Plus === Plus)
    assert(Minus === Minus)
    assert(Unstranded === Unstranded)
  }

  test("Invert orientation") {
    assert(Orientation.invert(Plus) === Minus)
    assert(Orientation.invert(Minus) === Plus)
    assert(Orientation.invert(Unstranded) === Unstranded)
  }

  test("Consensus orientation") {
    assert(consensus(Plus, Plus) === Plus)
    assert(consensus(Plus, Unstranded) === Plus)
    assert(consensus(Minus, Minus) === Minus)
    assert(consensus(Minus, Unstranded) === Minus)
    assert(consensus(Unstranded, Plus) === Plus)
    assert(consensus(Unstranded, Minus) === Minus)
    assert(consensus(Unstranded, Unstranded) === Unstranded)
    assert(consensus(Plus, Unstranded) === Plus)
    assert(consensus(Minus, Unstranded) === Minus)
  }

  test("Can overlap") {
    assert(isCompatible(Plus, Plus))
    assert(!isCompatible(Plus, Minus))
    assert(isCompatible(Plus, Unstranded))
    assert(!isCompatible(Minus, Plus))
    assert(isCompatible(Minus, Minus))
    assert(isCompatible(Minus, Unstranded))
    assert(isCompatible(Unstranded, Plus))
    assert(isCompatible(Unstranded, Minus))
    assert(isCompatible(Unstranded, Unstranded))
    assert(isCompatible(chr1_1900_2000_3000_4000_plus, chr1_1900_2000_3000_4000_plus))
    assert(!isCompatible(chr1_1900_2000_3000_4000_plus, chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus))
    assert(isCompatible(chr1_1900_2000_3000_4000_plus, chr1_1000_2000_3000_4000_5000_6000_7000_8000_both))
    assert(!isCompatible(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, chr1_1900_2000_3000_4000_plus))
    assert(isCompatible(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus))
    assert(isCompatible(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, chr1_1000_2000_3000_4000_5000_6000_7000_8000_both))
    assert(isCompatible(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, chr1_1900_2000_3000_4000_plus))
    assert(isCompatible(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus))
    assert(isCompatible(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, chr1_1000_2000_3000_4000_5000_6000_7000_8000_both))
    assert(!isCompatible(Empty, chr1_1900_2000_3000_4000_plus))
    assert(!isCompatible(Empty, chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus))
    assert(!isCompatible(Empty, chr1_1000_2000_3000_4000_5000_6000_7000_8000_both))
    assert(!isCompatible(chr1_1900_2000_3000_4000_plus, Empty))
    assert(!isCompatible(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, Empty))
    assert(!isCompatible(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, Empty))
    assert(!isCompatible(Empty, Empty))

  }

  test("Arbitrary ordering") {
    assert(ArbitraryOrdering.compare(Plus, Plus) === 0)
    assert(ArbitraryOrdering.compare(Plus, Minus) < 0)
    assert(ArbitraryOrdering.compare(Plus, Unstranded) < 0)
    assert(ArbitraryOrdering.compare(Minus, Plus) > 0)
    assert(ArbitraryOrdering.compare(Minus, Minus) === 0)
    assert(ArbitraryOrdering.compare(Minus, Unstranded) < 0)
    assert(ArbitraryOrdering.compare(Unstranded, Plus) > 0)
    assert(ArbitraryOrdering.compare(Unstranded, Minus) > 0)
    assert(ArbitraryOrdering.compare(Unstranded, Unstranded) === 0)
  }

  test("From string") {
    assert(Orientation.fromString("+") === Plus)
    assert(Orientation.fromString("-") === Minus)
    assert(Orientation.fromString("unstranded") === Unstranded)
    intercept[IllegalArgumentException](Orientation.fromString("wrong"))
  }

}
