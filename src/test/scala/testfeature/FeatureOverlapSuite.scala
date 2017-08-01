package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureOverlapSuite extends FunSuite {

  test("Generic feature overlaps generic feature") {
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlaps(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(!new GenericFeature(chr1_100_200_300_400_plus, Some("name")).overlaps(new GenericFeature(chr1_100_200_300_400_minus, Some("name"))))
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlapsSpan(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlapsSpan(new GenericFeature(
      Block("1", 1150, 1160, Plus), Some("name"))))
    assert(!new GenericFeature(chr1_100_200_300_400_plus, Some("name")).overlapsSpan(new GenericFeature(chr1_100_200_300_400_minus, Some("name"))))
  }

  test("Generic feature overlaps transcript") {
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlaps(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(!new GenericFeature(chr1_100_200_300_400_plus, Some("name")).overlaps(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))))
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).overlaps(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(!new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).overlaps(new GenericFeature(chr1_100_200_300_400_minus, Some("name"))))
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlapsSpan(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(!new GenericFeature(chr1_100_200_300_400_plus, Some("name")).overlapsSpan(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))))
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).overlapsSpan(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(new Transcript(Block("1", 1150, 1160, Plus), Some("name"), Some("gene")).overlapsSpan(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(!new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).overlapsSpan(new GenericFeature(chr1_100_200_300_400_minus, Some("name"))))
  }

  test("Generic feature overlaps mRNA") {
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlaps(MessengerRNA(chr1_1000_1100_1900_2000_plus, 1000, 1099, Some("name"), Some("gene"))))
    assert(!new GenericFeature(chr1_100_200_300_400_plus, Some("name")).overlaps(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene"))))
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene")).overlaps(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(!MessengerRNA(chr1_100_200_300_400_plus,100, 199, Some("name"), Some("gene")).overlaps(new GenericFeature(chr1_100_200_300_400_minus, Some("name"))))
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlapsSpan(MessengerRNA(chr1_1000_1100_1900_2000_plus, 1000, 1099, Some("name"), Some("gene"))))
    assert(!new GenericFeature(chr1_100_200_300_400_plus, Some("name")).overlapsSpan(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene"))))
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene")).overlapsSpan(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene")).overlapsSpan(new GenericFeature(
      Block("1", 1150, 1160, Plus), Some("name"))))
    assert(!MessengerRNA(chr1_100_200_300_400_plus,100, 199, Some("name"), Some("gene")).overlapsSpan(new GenericFeature(chr1_100_200_300_400_minus, Some("name"))))
  }

  test("Transcript overlaps transcript") {
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).overlaps(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(!new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).overlaps(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))))
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).overlapsSpan(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(new Transcript(Block("1", 1150, 1160, Plus), Some("name"), Some("gene")).overlapsSpan(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(!new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).overlapsSpan(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))))
  }

  test("Transcript overlaps mRNA") {
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).overlaps(MessengerRNA(chr1_1000_1100_1900_2000_plus, 1000, 1099, Some("name"), Some("gene"))))
    assert(!new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).overlaps(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene"))))
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene")).overlaps(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(!MessengerRNA(chr1_100_200_300_400_plus,100, 199, Some("name"), Some("gene")).overlaps(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))))
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).overlapsSpan(MessengerRNA(chr1_1000_1100_1900_2000_plus, 1000, 1099, Some("name"), Some("gene"))))
    assert(!new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).overlapsSpan(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene"))))
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene")).overlapsSpan(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(MessengerRNA(Block("1", 1150, 1160, Plus), 1151, 1157, Some("name"), Some("gene")).overlapsSpan(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(!MessengerRNA(chr1_100_200_300_400_plus,100, 199, Some("name"), Some("gene")).overlapsSpan(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))))
  }

  test("mRNA overlaps mRNA") {
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene"))
      .overlaps(MessengerRNA(chr1_1000_1100_1900_2000_plus, 1000, 1099, Some("name"), Some("gene"))))
    assert(!MessengerRNA(chr1_100_200_300_400_plus, 100, 199, Some("name"), Some("gene"))
      .overlaps(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene"))))
   assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene"))
      .overlapsSpan(MessengerRNA(chr1_1000_1100_1900_2000_plus, 1000, 1099, Some("name"), Some("gene"))))
   assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene"))
      .overlapsSpan(MessengerRNA(Block("1", 1150, 1160, Plus), 1151, 1157, Some("name"), Some("gene"))))
    assert(!MessengerRNA(chr1_100_200_300_400_plus, 100, 199, Some("name"), Some("gene"))
      .overlapsSpan(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene"))))
  }

  test("Overlap compatible introns - generic feature, generic feature") {
    assert(new GenericFeature(chr1_900_2100_3000_4000_plus, None)
      .overlapsCompatibleIntrons(new GenericFeature(chr1_1000_2000_both, None)))
    assert(!new GenericFeature(chr1_1000_1100_1200_1300_1900_2000_plus, None)
      .overlapsCompatibleIntrons(new GenericFeature(chr1_1000_2000_both, None)))
  }

  test("Overlap compatible introns - generic feature, transcript") {
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, None)
      .overlapsCompatibleIntrons(new Transcript(Block("1", 2200, 2350, Plus), None, None)))
    assert(!new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, None, None)
      .overlapsCompatibleIntrons(new GenericFeature(Block("1", 1400, 1750, Plus), None)))
  }

  test("Overlap compatible introns - generic feature, mRNA") {
    assert(new GenericFeature(chr1_1000_2000_3000_4000_plus, None)
      .overlapsCompatibleIntrons(MessengerRNA(chr1_1000_2000_plus, 1000, 1006, None, None)))
    assert(!MessengerRNA(chr1_1500_1600_2100_2300_plus, 2100, 2112, None, None)
      .overlapsCompatibleIntrons(new GenericFeature(chr1_1000_2000_both, None)))
  }

  test("Overlap compatible introns - transcript, transcript") {
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, None, None)
      .overlapsCompatibleIntrons(new Transcript(Block("1", 1400, 1600, Plus), None, None)))
    assert(!new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, None, None)
      .overlapsCompatibleIntrons(new Transcript(Block("1", 1500, 1650, Plus), None, None)))
  }

  test("Overlap compatible introns - transcript, mRNA") {
    assert(!new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, None, None)
      .overlapsCompatibleIntrons(MessengerRNA(Block("1", 2000, 2300, Plus), 2000, 2012, None, None)))
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1700, 1712, None, None)
      .overlapsCompatibleIntrons(new Transcript(Block("1", 2100, 2400, Plus), None, None)))
  }

  test("Overlap compatible introns - mRNA, mRNA") {
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1512, None, None)
      .overlapsCompatibleIntrons(MessengerRNA(Block("1", 2100, 2300, Plus), 2100, 2112, None, None)))
    assert(!MessengerRNA(chr1_900_1100_1500_1600_1900_2100_plus, 900, 912, None, None)
      .overlapsCompatibleIntrons(MessengerRNA(chr1_1000_2000_plus, 1000, 1012, None, None)))
  }



}
