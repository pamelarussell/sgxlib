package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureIntersectSuite extends FunSuite {

  test("Generic feature intersect generic feature") {
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(new GenericFeature(Block("chr1", 1550, 1650, Plus), Some("name"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(new GenericFeature(Block("chr1", 1550, 1650, Plus), Some("name"))
      .intersection(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(new GenericFeature(Block("chr1", 2300, 2400, Plus), Some("name"))) === None)
    assert(new GenericFeature(Block("chr1", 2300, 2400, Plus), Some("name"))
      .intersection(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))) === None)
  }

  test("Generic feature intersect transcript") {
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(new Transcript(Block("chr1", 1550, 1650, Plus), Some("name"), Some("gene"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(new Transcript(Block("chr1", 1550, 1650, Plus), Some("name"), Some("gene"))
      .intersection(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(new GenericFeature(Block("chr1", 2300, 2400, Plus), Some("name"))) === None)
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(new Transcript(Block("chr1", 2300, 2400, Plus), Some("name"), Some("gene"))) === None)
  }

  test("Generic feature intersect mRNA") {
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(MessengerRNA(Block("chr1", 1550, 1650, Plus), 1550, 1650, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(Block("chr1", 1550, 1650, Plus), 1550, 1650, Some("name"), Some("gene"))
      .intersection(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1600, Some("name"), Some("gene"))
      .intersection(new GenericFeature(Block("chr1", 2300, 2400, Plus), Some("name"))) === None)
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(MessengerRNA(Block("chr1", 2300, 2400, Plus), 2300, 2400, Some("name"), Some("gene"))) === None)
  }

  test("Transcript intersect transcript") {
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(new Transcript(Block("chr1", 1550, 1650, Plus), Some("name"), Some("gene"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(new Transcript(Block("chr1", 1550, 1650, Plus), Some("name"), Some("gene"))
      .intersection(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(new Transcript(Block("chr1", 2300, 2400, Plus), Some("name"), Some("gene"))) === None)
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(new Transcript(Block("chr1", 2300, 2400, Plus), Some("name"), Some("gene"))) === None)
  }

  test("Transcript intersect mRNA") {
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("chr1", 1550, 1650, Plus), 1550, 1650, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(Block("chr1", 1550, 1650, Plus), 1550, 1650, Some("name"), Some("gene"))
      .intersection(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1600, Some("name"), Some("gene"))
      .intersection(new Transcript(Block("chr1", 2300, 2400, Plus), Some("name"), Some("gene"))) === None)
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("chr1", 2300, 2400, Plus), 2300, 2400, Some("name"), Some("gene"))) === None)
  }

  test("mRNA intersect mRNA") {
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1600, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("chr1", 1550, 1650, Plus), 1550, 1650, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(Block("chr1", 1550, 1650, Plus), 1550, 1650, Some("name"), Some("gene"))
      .intersection(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1600, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("chr1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1600, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("chr1", 2300, 2400, Plus), 2300, 2400, Some("name"), Some("gene"))) === None)
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1600, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("chr1", 2300, 2400, Plus), 2300, 2400, Some("name"), Some("gene"))) === None)
  }

}
