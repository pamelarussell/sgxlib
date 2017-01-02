package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureIntersectSuite extends FunSuite {

  test("Generic feature intersect generic feature") {
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(new GenericFeature(Block("1", 1550, 1650, Plus), Some("name"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(new GenericFeature(Block("1", 1550, 1650, Plus), Some("name"))
      .intersection(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(new GenericFeature(Block("1", 2300, 2400, Plus), Some("name"))) === None)
    assert(new GenericFeature(Block("1", 2300, 2400, Plus), Some("name"))
      .intersection(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))) === None)
  }

  test("Generic feature intersect transcript") {
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(new Transcript(Block("1", 1550, 1650, Plus), Some("name"), Some("gene"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(new Transcript(Block("1", 1550, 1650, Plus), Some("name"), Some("gene"))
      .intersection(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(new GenericFeature(Block("1", 2300, 2400, Plus), Some("name"))) === None)
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(new Transcript(Block("1", 2300, 2400, Plus), Some("name"), Some("gene"))) === None)
  }

  test("Generic feature intersect mRNA") {
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(MessengerRNA(Block("1", 1550, 1650, Plus), 1550, 1580, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(Block("1", 1550, 1650, Plus), 1550, 1580, Some("name"), Some("gene"))
      .intersection(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1590, Some("name"), Some("gene"))
      .intersection(new GenericFeature(Block("1", 2300, 2400, Plus), Some("name"))) === None)
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"))
      .intersection(MessengerRNA(Block("1", 2300, 2400, Plus), 2300, 2399, Some("name"), Some("gene"))) === None)
  }

  test("Transcript intersect transcript") {
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(new Transcript(Block("1", 1550, 1650, Plus), Some("name"), Some("gene"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(new Transcript(Block("1", 1550, 1650, Plus), Some("name"), Some("gene"))
      .intersection(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(new Transcript(Block("1", 2300, 2400, Plus), Some("name"), Some("gene"))) === None)
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(new Transcript(Block("1", 2300, 2400, Plus), Some("name"), Some("gene"))) === None)
  }

  test("Transcript intersect mRNA") {
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("1", 1550, 1650, Plus), 1550, 1580, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(Block("1", 1550, 1650, Plus), 1550, 1580, Some("name"), Some("gene"))
      .intersection(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1590, Some("name"), Some("gene"))
      .intersection(new Transcript(Block("1", 2300, 2400, Plus), Some("name"), Some("gene"))) === None)
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("1", 2300, 2400, Plus), 2300, 2399, Some("name"), Some("gene"))) === None)
  }

  test("mRNA intersect mRNA") {
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1599, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("1", 1550, 1650, Plus), 1550, 1580, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(Block("1", 1550, 1650, Plus), 1550, 1580, Some("name"), Some("gene"))
      .intersection(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1599, Some("name"), Some("gene"))) === Some(new GenericFeature(Block("1", 1550, 1600, Plus), None)))
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1599, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("1", 2300, 2400, Plus), 2300, 2399, Some("name"), Some("gene"))) === None)
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1599, Some("name"), Some("gene"))
      .intersection(MessengerRNA(Block("1", 2300, 2400, Plus), 2300, 2399, Some("name"), Some("gene"))) === None)
  }

}
