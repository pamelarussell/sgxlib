package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureMinusSuite extends FunSuite {

  val bs = BlockSet(List(
    Block("chr1", 900, 1000, Plus),
    Block("chr1", 2000, 2100, Plus),
    Block("chr1", 3000, 4000, Plus)
  ))

  test("Generic feature minus generic feature") {
    assert(new GenericFeature(chr1_900_2100_3000_4000_plus, Some("name"))
      .minus(new GenericFeature(chr1_1000_2000_plus, Some("name"))) === Some(new GenericFeature(bs, None)))
    assert(new GenericFeature(chr1_1100_1200_1300_1400_plus, Some("name"))
      .minus(new GenericFeature(chr1_1000_2000_plus, Some("name"))) === None)
  }

  test("Generic feature minus transcript") {
    assert(new GenericFeature(chr1_900_2100_3000_4000_plus, Some("name"))
      .minus(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))) === Some(new GenericFeature(bs, None)))
    assert(new GenericFeature(chr1_1100_1200_1300_1400_plus, Some("name"))
      .minus(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))) === None)
  }

  test("Generic feature minus mRNA") {
    assert(new GenericFeature(chr1_900_2100_3000_4000_plus, Some("name"))
      .minus(MessengerRNA(chr1_1000_2000_plus, 1000, 1099, Some("name"), Some("gene"))) === Some(new GenericFeature(bs, None)))
    assert(new GenericFeature(chr1_1100_1200_1300_1400_plus, Some("name"))
      .minus(MessengerRNA(chr1_1000_2000_plus, 1000, 1099, Some("name"), Some("gene"))) === None)
  }

  test("Transcript minus generic feature") {
    assert(new Transcript(chr1_900_2100_3000_4000_plus, Some("name"), Some("gene"))
      .minus(new GenericFeature(chr1_1000_2000_plus, Some("name"))) === Some(new GenericFeature(bs, None)))
    assert(new Transcript(chr1_1100_1200_1300_1400_plus, Some("name"), Some("gene"))
      .minus(new GenericFeature(chr1_1000_2000_plus, Some("name"))) === None)
  }

  test("Transcript minus transcript") {
    assert(new Transcript(chr1_900_2100_3000_4000_plus, Some("name"), Some("gene"))
      .minus(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))) === Some(new GenericFeature(bs, None)))
    assert(new Transcript(chr1_1100_1200_1300_1400_plus, Some("name"), Some("gene"))
      .minus(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))) === None)
  }

  test("Transcript minus mRNA") {
    assert(new Transcript(chr1_900_2100_3000_4000_plus, Some("name"), Some("gene"))
      .minus(MessengerRNA(chr1_1000_2000_plus, 1000, 1099, Some("name"), Some("gene"))) === Some(new GenericFeature(bs, None)))
    assert(new Transcript(chr1_1100_1200_1300_1400_plus, Some("name"), Some("gene"))
      .minus(MessengerRNA(chr1_1000_2000_plus, 1000, 1099, Some("name"), Some("gene"))) === None)
  }

  test("mRNA minus generic feature") {
    assert(MessengerRNA(chr1_900_2100_3000_4000_plus, 900, 999, Some("name"), Some("gene"))
      .minus(new GenericFeature(chr1_1000_2000_plus, Some("name"))) === Some(new GenericFeature(bs, None)))
    assert(MessengerRNA(chr1_1100_1200_1300_1400_plus, 1100, 1199, Some("name"), Some("gene"))
      .minus(new GenericFeature(chr1_1000_2000_plus, Some("name"))) === None)
  }

  test("mRNA minus transcript") {
    assert(MessengerRNA(chr1_900_2100_3000_4000_plus, 900, 999, Some("name"), Some("gene"))
      .minus(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))) === Some(new GenericFeature(bs, None)))
    assert(MessengerRNA(chr1_1100_1200_1300_1400_plus, 1100, 1199, Some("name"), Some("gene"))
      .minus(new Transcript(chr1_1000_2000_plus, Some("name"), Some("gene"))) === None)
  }

  test("mRNA minus mRNA") {
    assert(MessengerRNA(chr1_900_2100_3000_4000_plus, 900, 999, Some("name"), Some("gene"))
      .minus(MessengerRNA(chr1_1000_2000_plus, 1000, 1099, Some("name"), Some("gene"))) === Some(new GenericFeature(bs, None)))
    assert(MessengerRNA(chr1_1100_1200_1300_1400_plus, 1100, 1199, Some("name"), Some("gene"))
      .minus(MessengerRNA(chr1_1000_2000_plus, 1000, 1099, Some("name"), Some("gene"))) === None)
  }

}
