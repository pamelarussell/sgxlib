package testfeature

import feature.{GenericFeature, MessengerRNA, Transcript}
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureOverlapSuite extends FunSuite {

  test("Generic feature overlaps generic feature") {
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlaps(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(!new GenericFeature(chr1_100_200_300_400_plus, Some("name")).overlaps(new GenericFeature(chr1_100_200_300_400_minus, Some("name"))))
  }

  test("Generic feature overlaps transcript") {
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlaps(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(!new GenericFeature(chr1_100_200_300_400_plus, Some("name")).overlaps(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))))
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).overlaps(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(!new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).overlaps(new GenericFeature(chr1_100_200_300_400_minus, Some("name"))))
  }

  test("Generic feature overlaps mRNA") {
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).overlaps(MessengerRNA(chr1_1000_1100_1900_2000_plus, 1000, 1099, Some("name"), Some("gene"))))
    assert(!new GenericFeature(chr1_100_200_300_400_plus, Some("name")).overlaps(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene"))))
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene")).overlaps(new GenericFeature(chr1_1000_1100_1900_2000_plus, Some("name"))))
    assert(!MessengerRNA(chr1_100_200_300_400_plus,100, 199, Some("name"), Some("gene")).overlaps(new GenericFeature(chr1_100_200_300_400_minus, Some("name"))))
  }

  test("Transcript overlaps transcript") {
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).overlaps(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(!new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).overlaps(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))))
  }

  test("Transcript overlaps mRNA") {
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).overlaps(MessengerRNA(chr1_1000_1100_1900_2000_plus, 1000, 1099, Some("name"), Some("gene"))))
    assert(!new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).overlaps(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene"))))
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene")).overlaps(new Transcript(chr1_1000_1100_1900_2000_plus, Some("name"), Some("gene"))))
    assert(!MessengerRNA(chr1_100_200_300_400_plus,100, 199, Some("name"), Some("gene")).overlaps(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene"))))
  }

  test("mRNA overlaps mRNA") {
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 900, 999, Some("name"), Some("gene"))
      .overlaps(MessengerRNA(chr1_1000_1100_1900_2000_plus, 1000, 1099, Some("name"), Some("gene"))))
    assert(!MessengerRNA(chr1_100_200_300_400_plus, 100, 199, Some("name"), Some("gene"))
      .overlaps(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene"))))
  }

}
