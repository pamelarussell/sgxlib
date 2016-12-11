package testfeature

import feature.{GenericFeature, MessengerRNA, Transcript}
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

}
