package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureLocationSuite extends FunSuite {

  test("Get chr, start, end, orientation") {
    assert(new GenericFeature(chr1_1700_1800_1900_2100_plus, Some("name")).getChr === "chr1")
    assert(new Transcript(chr1_1700_1800_1900_2100_plus, Some("name"), Some("gene")).getChr === "chr1")
    assert(MessengerRNA(chr1_1700_1800_1900_2100_plus, 1750, 1780, Some("name"), Some("gene")).getChr === "chr1")
    assert(new GenericFeature(chr1_1700_1800_1900_2100_plus, Some("name")).getStart === 1700)
    assert(new Transcript(chr1_1700_1800_1900_2100_plus, Some("name"), Some("gene")).getStart === 1700)
    assert(MessengerRNA(chr1_1700_1800_1900_2100_plus, 1750, 1780, Some("name"), Some("gene")).getStart === 1700)
    assert(new GenericFeature(chr1_1700_1800_1900_2100_plus, Some("name")).getEnd === 2100)
    assert(new Transcript(chr1_1700_1800_1900_2100_plus, Some("name"), Some("gene")).getEnd === 2100)
    assert(MessengerRNA(chr1_1700_1800_1900_2100_plus, 1750, 1780, Some("name"), Some("gene")).getEnd === 2100)
    assert(new GenericFeature(chr1_1700_1800_1900_2100_plus, Some("name")).getOrientation === Plus)
    assert(new Transcript(chr1_1700_1800_1900_2100_plus, Some("name"), Some("gene")).getOrientation === Plus)
    assert(MessengerRNA(chr1_1700_1800_1900_2100_plus, 1750, 1780, Some("name"), Some("gene")).getOrientation === Plus)
  }

  test("Get blocks") {
    assert(new GenericFeature(chr1_1700_1800_1900_2100_plus, Some("name")).getBlocks === List(
      Block("chr1", 1700, 1800, Plus),
      Block("chr1", 1900, 2100, Plus)
    ))
    assert(new Transcript(chr1_1700_1800_1900_2100_plus, Some("name"), Some("gene")).getBlocks === List(
      Block("chr1", 1700, 1800, Plus),
      Block("chr1", 1900, 2100, Plus)
    ))
    assert(MessengerRNA(chr1_1700_1800_1900_2100_plus, 1750, 1780, Some("name"), Some("gene")).getBlocks === List(
      Block("chr1", 1700, 1800, Plus),
      Block("chr1", 1900, 2100, Plus)
    ))
    assert(new GenericFeature(chr1_1000_2000_minus, Some("name")).getBlocks === List(Block("chr1", 1000, 2000, Minus)))
    assert(new Transcript(chr1_1000_2000_minus, Some("name"), Some("gene")).getBlocks === List(Block("chr1", 1000, 2000, Minus)))
    assert(MessengerRNA(chr1_1000_2000_minus, 1000, 1099, Some("name"), Some("gene")).getBlocks === List(Block("chr1", 1000, 2000, Minus)))
  }

}
