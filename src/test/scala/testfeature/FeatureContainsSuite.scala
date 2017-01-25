package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureContainsSuite extends FunSuite {

  test("Generic feature contains generic feature") {
    assert(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_plus, None)
      .contains(new GenericFeature(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus, None)))
    assert(!new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_plus, None).contains(new GenericFeature(chr1_6000_7000_8000_9000_plus, None)))
  }

  test("Generic feature contains transcript") {
    assert(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_plus, None)
      .contains(new Transcript(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus, None, None)))
    assert(!new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_plus, None).contains(new Transcript(chr1_6000_7000_8000_9000_plus, None, None)))
  }

  test("Generic feature contains mRNA") {
    assert(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_plus, None)
      .contains(MessengerRNA(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus, 1000, 1099, None, None)))
    assert(!new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_plus, None).contains(MessengerRNA(chr1_6000_7000_8000_9000_plus, 6000, 6999, None, None)))
  }

  test("Transcript contains generic feature") {
    assert(new Transcript(chr1_1000_2000_3000_4000_5000_6000_plus, None, None)
      .contains(new GenericFeature(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus, None)))
    assert(!new Transcript(chr1_1000_2000_3000_4000_5000_6000_plus, None, None).contains(new GenericFeature(chr1_6000_7000_8000_9000_plus, None)))
  }

  test("Transcript contains transcript") {
    assert(new Transcript(chr1_1000_2000_3000_4000_5000_6000_plus, None, None)
      .contains(new Transcript(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus, None, None)))
    assert(!new Transcript(chr1_1000_2000_3000_4000_5000_6000_plus, None, None).contains(new Transcript(chr1_6000_7000_8000_9000_plus, None, None)))
  }

  test("Transcript contains mRNA") {
    assert(new Transcript(chr1_1000_2000_3000_4000_5000_6000_plus, None, None)
      .contains(MessengerRNA(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus, 1000, 1099, None, None)))
    assert(!new Transcript(chr1_1000_2000_3000_4000_5000_6000_plus, None, None).contains(MessengerRNA(chr1_6000_7000_8000_9000_plus, 6000, 6099, None, None)))
  }

  test("mRNA contains generic feature") {
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_plus, 1000, 1099, None, None)
      .contains(new GenericFeature(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus, None)))
    assert(!MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_plus, 1000, 1099, None, None).contains(new GenericFeature(chr1_6000_7000_8000_9000_plus, None)))
  }

  test("mRNA contains transcript") {
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_plus, 1000, 1099, None, None)
      .contains(new Transcript(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus, None, None)))
    assert(!MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_plus, 1000, 1099, None, None).contains(new Transcript(chr1_6000_7000_8000_9000_plus, None, None)))
  }

  test("mRNA contains mRNA") {
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_plus, 1000, 1099, None, None)
      .contains(MessengerRNA(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus, 1000, 1099, None, None)))
    assert(!MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_plus, 1000, 1099, None, None)
      .contains(MessengerRNA(chr1_6000_7000_8000_9000_plus, 6000, 6999, None, None)))
  }

  test("Contains compatible introns - generic feature, generic feature") {
    assert(new GenericFeature(chr1_900_2100_3000_4000_plus, None)
      .containsCompatibleIntrons(new GenericFeature(chr1_1000_2000_both, None)))
    assert(!new GenericFeature(chr1_1000_1100_1200_1300_1900_2000_plus, None)
      .containsCompatibleIntrons(new GenericFeature(chr1_1000_2000_both, None)))
  }

  test("Contains compatible introns - generic feature, transcript") {
    assert(new GenericFeature(chr1_1500_1600_1700_1800_2100_2300_plus, None)
      .containsCompatibleIntrons(new Transcript(Block("1", 2200, 2300, Plus), None, None)))
    assert(!new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, None, None)
      .containsCompatibleIntrons(new GenericFeature(Block("1", 1400, 1750, Plus), None)))
  }

  test("Contains compatible introns - generic feature, mRNA") {
    assert(new GenericFeature(chr1_1000_2000_3000_4000_plus, None)
      .containsCompatibleIntrons(MessengerRNA(chr1_1000_2000_plus, 1000, 1006, None, None)))
    assert(!MessengerRNA(chr1_1500_1600_2100_2300_plus, 2100, 2112, None, None)
      .containsCompatibleIntrons(new GenericFeature(chr1_1000_2000_both, None)))
  }

  test("Contains compatible introns - transcript, transcript") {
    assert(new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, None, None)
      .containsCompatibleIntrons(new Transcript(BlockSet(List(Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1750, Plus))), None, None)))
    assert(!new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, None, None)
      .containsCompatibleIntrons(new Transcript(Block("1", 1500, 1650, Plus), None, None)))
  }

  test("Contains compatible introns - transcript, mRNA") {
    assert(!new Transcript(chr1_1500_1600_1700_1800_2100_2300_plus, None, None)
      .containsCompatibleIntrons(MessengerRNA(Block("1", 2000, 2300, Plus), 2000, 2012, None, None)))
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1700, 1712, None, None)
      .containsCompatibleIntrons(new Transcript(Block("1", 2100, 2300, Plus), None, None)))

  }

  test("Contains compatible introns - mRNA, mRNA") {
    assert(MessengerRNA(chr1_1500_1600_1700_1800_2100_2300_plus, 1500, 1512, None, None)
      .containsCompatibleIntrons(MessengerRNA(Block("1", 2100, 2300, Plus), 2100, 2112, None, None)))
    assert(!MessengerRNA(chr1_900_1100_1500_1600_1900_2100_plus, 900, 912, None, None)
      .containsCompatibleIntrons(MessengerRNA(chr1_1000_2000_plus, 1000, 1012, None, None)))

  }


}
