package testfeature

import feature.{Block, Minus, Plus, Transcript}
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class TranscriptMiscSuite extends FunSuite {

  test("Get introns") {
    assert(new Transcript(chr1_1000_2000_minus, Some("name"), Some("gene"))
      .getIntrons === Nil)
    assert(new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene"))
      .getIntrons === List(Block("1", 200, 300, Plus)))
    assert(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))
      .getIntrons === List(Block("1", 200, 300, Minus)))
    assert(new Transcript(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, Some("name"), Some("gene"))
      .getIntrons === List(
      Block("1", 2000, 3000, Plus),
      Block("1", 4000, 5000, Plus),
      Block("1", 6000, 7000, Plus)))
    assert(new Transcript(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, Some("name"), Some("gene"))
      .getIntrons === List(
      Block("1", 2000, 3000, Minus),
      Block("1", 4000, 5000, Minus),
      Block("1", 6000, 7000, Minus)))
  }

  test("Illegal orientation") {
    intercept[IllegalArgumentException](new Transcript(chr1_1000_2000_both, Some("name"), Some("gene")))
    intercept[IllegalArgumentException](new Transcript(chr1_1000_2000_unstranded, Some("name"), Some("gene")))
  }

}
