package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureAddBlockSuite extends FunSuite {

  test("Generic feature add block") {
    intercept[IllegalArgumentException](new GenericFeature(chr1_1000_2000_minus, Some("name")).addBlock(chr1_1000_2000_plus_1))
    intercept[IllegalArgumentException](new GenericFeature(chr1_1000_2000_plus_1, Some("name")).addBlock(chr2_3000_4000_plus))
    assert(new GenericFeature(chr2_1000_2000_plus, Some("name")).addBlock(chr2_3000_4000_plus) ===
      new GenericFeature(BlockSet(List(
      Block("chr2", 1000, 2000, Plus),
      Block("chr2", 3000, 4000, Plus))), None))
    intercept[IllegalArgumentException](new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name")).addBlock(chr1_1000_2000_minus))
    intercept[IllegalArgumentException](new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, Some("name")).addBlock(chr2_1000_2000_plus))
    assert(new GenericFeature(chr1_100_200_300_400_plus, Some("name")).addBlock(chr1_1000_2000_plus_1) === new GenericFeature(BlockSet(List(
      Block("chr1", 100, 200, Plus),
      Block("chr1", 300, 400, Plus),
      Block("chr1", 1000, 2000, Plus)
    )), None))
    assert(new GenericFeature(chr1_900_1100_1500_1600_1900_2100_plus, Some("name")).addBlock(chr1_1000_2000_both) ===
      new GenericFeature(Block("chr1", 900, 2100, Plus), None))
  }

  test("Transcript add block") {
    intercept[IllegalArgumentException](new Transcript(chr1_1000_2000_minus, Some("name"), Some("gene")).addBlock(chr1_1000_2000_plus_1))
    intercept[IllegalArgumentException](new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")).addBlock(chr2_3000_4000_plus))
    assert(new Transcript(chr2_1000_2000_plus, Some("name"), Some("gene")).addBlock(chr2_3000_4000_plus) ===
      new GenericFeature(BlockSet(List(
        Block("chr2", 1000, 2000, Plus),
        Block("chr2", 3000, 4000, Plus))), None))
    intercept[IllegalArgumentException](new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene")).addBlock(chr1_1000_2000_minus))
    intercept[IllegalArgumentException](new Transcript(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, Some("name"), Some("gene")).addBlock(chr2_1000_2000_plus))
    assert(new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).addBlock(chr1_1000_2000_plus_1) === new GenericFeature(BlockSet(List(
      Block("chr1", 100, 200, Plus),
      Block("chr1", 300, 400, Plus),
      Block("chr1", 1000, 2000, Plus)
    )), None))
    assert(new Transcript(chr1_900_1100_1500_1600_1900_2100_plus, Some("name"), Some("gene")).addBlock(chr1_1000_2000_both) ===
      new GenericFeature(Block("chr1", 900, 2100, Plus), None))
  }

  test("mRNA add block") {
    intercept[IllegalArgumentException](MessengerRNA(chr1_1000_2000_minus, 1000, 1012, Some("name"), Some("gene"))
      .addBlock(chr1_1000_2000_plus_1))
    intercept[IllegalArgumentException](MessengerRNA(chr1_1000_2000_plus_1, 1000, 1012, Some("name"), Some("gene"))
      .addBlock(chr2_3000_4000_plus))
    assert(MessengerRNA(chr2_1000_2000_plus, 1000, 1012, Some("name"), Some("gene")).addBlock(chr2_3000_4000_plus) ===
      new GenericFeature(BlockSet(List(
        Block("chr2", 1000, 2000, Plus),
        Block("chr2", 3000, 4000, Plus))), None))
    intercept[IllegalArgumentException](MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1512, Some("name")
      , Some("gene")).addBlock(chr1_1000_2000_minus))
    intercept[IllegalArgumentException](MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1012
      , Some("name"), Some("gene")).addBlock(chr2_1000_2000_plus))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 100, 112, Some("name"), Some("gene"))
      .addBlock(chr1_1000_2000_plus_1) === new GenericFeature(BlockSet(List(
      Block("chr1", 100, 200, Plus),
      Block("chr1", 300, 400, Plus),
      Block("chr1", 1000, 2000, Plus)
    )), None))
    assert(MessengerRNA(chr1_900_1100_1500_1600_1900_2100_plus, 1000, 1012, Some("name"), Some("gene"))
      .addBlock(chr1_1000_2000_both) ===
      new GenericFeature(Block("chr1", 900, 2100, Plus), None))
  }

}
