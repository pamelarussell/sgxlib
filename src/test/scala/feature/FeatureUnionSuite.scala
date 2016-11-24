package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureUnionSuite extends FunSuite {

  val bs = BlockSet(List(
    Block("chr1", 900, 2100, Plus),
    Block("chr1", 3000, 4000, Plus)
  ))

  test("Generic feature union generic feature") {
    assert(new GenericFeature(chr1_1000_2000_plus, Some("name"))
      .union(new GenericFeature(chr1_900_2100_3000_4000_plus, Some("name"))) === new GenericFeature(bs, None))
    assert(new GenericFeature(chr1_900_2100_3000_4000_plus, Some("name"))
      .union(new GenericFeature(chr1_1000_2000_plus, Some("name"))) === new GenericFeature(bs, None))
  }

  test("Generic feature union transcript") {
    assert(new GenericFeature(chr1_1000_2000_plus, Some("name"))
      .union(new Transcript(chr1_900_2100_3000_4000_plus, Some("name"), Some("gene"))) === new GenericFeature(bs, None))
    assert(new Transcript(chr1_900_2100_3000_4000_plus, Some("name"), Some("gene"))
      .union(new GenericFeature(chr1_1000_2000_plus, Some("name"))) === new GenericFeature(bs, None))
  }

  test("Generic feature union mRNA") {
    assert(new GenericFeature(chr1_1000_2000_plus, Some("name"))
      .union(MessengerRNA(chr1_900_2100_3000_4000_plus, 900, 999, Some("name"), Some("gene"))) === new GenericFeature(bs, None))
    assert(MessengerRNA(chr1_900_2100_3000_4000_plus, 900, 999, Some("name"), Some("gene"))
      .union(new GenericFeature(chr1_1000_2000_plus, Some("name"))) === new GenericFeature(bs, None))
  }

  test("Transcript union transcript") {
    assert(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))
      .union(new Transcript(chr1_900_2100_3000_4000_plus, Some("name"), Some("gene"))) === new GenericFeature(bs, None))
    assert(new Transcript(chr1_900_2100_3000_4000_plus, Some("name"), Some("gene"))
      .union(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))) === new GenericFeature(bs, None))
  }

  test("Transcript union mRNA") {
    assert(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))
      .union(MessengerRNA(chr1_900_2100_3000_4000_plus, 900, 999, Some("name"), Some("gene"))) === new GenericFeature(bs, None))
    assert(MessengerRNA(chr1_900_2100_3000_4000_plus, 900, 999, Some("name"), Some("gene"))
      .union(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))) === new GenericFeature(bs, None))
  }

  test("mRNA union mRNA") {
    assert(MessengerRNA(chr1_1000_2000_plus, 1000, 1300, Some("name"), Some("gene"))
      .union(MessengerRNA(chr1_900_2100_3000_4000_plus, 900, 999, Some("name"), Some("gene"))) === new GenericFeature(bs, None))
    assert(MessengerRNA(chr1_900_2100_3000_4000_plus, 900, 999, Some("name"), Some("gene"))
      .union(MessengerRNA(chr1_1000_2000_plus, 1000, 1300, Some("name"), Some("gene"))) === new GenericFeature(bs, None))
  }

}
