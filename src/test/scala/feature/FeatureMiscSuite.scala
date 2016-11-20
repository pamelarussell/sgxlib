package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureMiscSuite extends FunSuite {

  test("GenericFeature equals and hashCode") {
    // Equals
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) === new GenericFeature(chr1_1000_2000_plus_2, Some("name")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) === new GenericFeature(chr1_1000_2000_plus_2, None))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) === new GenericFeature(chr1_1000_2000_plus_1, None))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) != new GenericFeature(chr1_1000_2000_plus_1, Some("name")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) != new GenericFeature(chr1_1500_2500_minus, Some("name")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) equals new GenericFeature(chr1_1000_2000_plus_2, Some("name")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) equals new GenericFeature(chr1_1000_2000_plus_2, None))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) equals new GenericFeature(chr1_1000_2000_plus_1, None))
    assert(!(new GenericFeature(chr1_1000_2000_plus_1, None) equals new GenericFeature(chr1_1000_2000_plus_1, Some("name"))))
    assert(!(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) equals new GenericFeature(chr1_1500_2500_minus, Some("name"))))
    // Hash code
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")).hashCode === new GenericFeature(chr1_1000_2000_plus_2, Some("name")).hashCode)
    assert(new GenericFeature(chr1_1000_2000_plus_1, None).hashCode === new GenericFeature(chr1_1000_2000_plus_2, None).hashCode)
    assert(new GenericFeature(chr1_1000_2000_plus_1, None).hashCode === new GenericFeature(chr1_1000_2000_plus_1, None).hashCode)
  }

  test("Transcript equals and hashCode") {
    // Equals
    assert(new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")) === new Transcript(chr1_1000_2000_plus_2, Some("name"), Some("gene")))
    assert(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) === new Transcript(chr1_1000_2000_plus_2, None, Some("gene")))
    assert(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) === new Transcript(chr1_1000_2000_plus_1, None, Some("gene")))
    assert(new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")) != new Transcript(chr1_1000_2000_plus_2, Some("name"), Some("gene2")))
    assert(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) != new Transcript(chr1_1000_2000_plus_2, None, Some("gene2")))
    assert(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) != new Transcript(chr1_1000_2000_plus_1, None, Some("gene2")))
    assert(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) != new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")))
    assert(new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")) != new Transcript(chr1_1500_2500_minus, Some("name"), Some("gene")))
    assert(new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")) equals new Transcript(chr1_1000_2000_plus_2, Some("name"), Some("gene")))
    assert(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) equals new Transcript(chr1_1000_2000_plus_2, None, Some("gene")))
    assert(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) equals new Transcript(chr1_1000_2000_plus_1, None, Some("gene")))
    assert(!(new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")) equals new Transcript(chr1_1000_2000_plus_2, Some("name"), Some("gene2"))))
    assert(!(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) equals new Transcript(chr1_1000_2000_plus_2, None, Some("gene2"))))
    assert(!(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) equals new Transcript(chr1_1000_2000_plus_1, None, Some("gene2"))))
    assert(!(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")) equals new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene"))))
    assert(!(new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")) equals new Transcript(chr1_1500_2500_minus, Some("name"), Some("gene"))))
    // Hash code
    assert(new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")).hashCode === new Transcript(chr1_1000_2000_plus_2, Some("name"), Some("gene")).hashCode)
    assert(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")).hashCode === new Transcript(chr1_1000_2000_plus_2, None, Some("gene")).hashCode)
    assert(new Transcript(chr1_1000_2000_plus_1, None, Some("gene")).hashCode === new Transcript(chr1_1000_2000_plus_1, None, Some("gene")).hashCode)
  }

  test("Messenger RNA equals and hashCode") {
    val mr1 = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1600, Some("name"), Some("gene"))
    val mr2 = MessengerRNA(chr1_1000_2000_plus_2, 1500, 1600, Some("name"), Some("gene"))
    val mr3 = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1700, Some("name"), Some("gene"))
    val mr4 = MessengerRNA(chr1_1000_2000_plus_1, 1400, 1600, Some("name"), Some("gene"))
    val mr5 = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1600, Some("name2"), Some("gene"))
    val mr6 = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1600, Some("name"), Some("gene2"))
    val mr7 = MessengerRNA(chr1_900_2100_3000_4000_plus, 1500, 1600, Some("name"), Some("gene"))
    assert(mr1.hashCode === mr2.hashCode)
    assert(mr1 === mr2)
    assert(mr1 != mr3)
    assert(mr3 != mr1)
    assert(mr1 != mr4)
    assert(mr4 != mr1)
    assert(mr1 != mr5)
    assert(mr5 != mr1)
    assert(mr1 != mr6)
    assert(mr6 != mr1)
    assert(mr1 != mr7)
    assert(mr7 != mr1)
  }

  test("Inter-class equals") {
    val gf = new GenericFeature(chr1_1000_2000_plus_1, Some("name"))
    val tr = new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene"))
    val mr = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1600, Some("name"), Some("gene"))
    val mr2 = MessengerRNA(chr1_1000_2000_plus_1, 1000, 1500, Some("name"), Some("gene"))
    assert(gf != tr)
    assert(gf != mr)
    assert(gf != mr2)
    assert(tr != gf)
    assert(tr != mr)
    assert(tr != mr2)
    assert(mr != gf)
    assert(mr2 != gf)
    assert(mr != tr)
    assert(mr2 != tr)
    assert(mr != mr2)
    assert(mr2 != mr)
  }

  test("Transcript and mRNA valid orientation") {
    val b1 = Block("chr1", 1000, 2000, Unstranded)
    val b2 = Block("chr1", 3000, 4000, Unstranded)
    val b3 = Block("chr1", 1000, 2000, Both)
    val b4 = Block("chr1", 3000, 4000, Both)
    val bs1 = BlockSet(List(b1, b2))
    val bs2 = BlockSet(List(b3, b4))
    intercept[IllegalArgumentException]{ new Transcript(b1, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ new Transcript(b3, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ new Transcript(bs1, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ new Transcript(bs2, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(b1, 1500, 1600, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(b3, 1500, 1600, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(bs1, 1500, 1600, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(bs2, 1500, 1600, Some("name"), Some("gene")) }
  }

  test("Num blocks") {
    assert(new GenericFeature(Block("chr1", 1000, 2000, Plus), Some("name")).numBlocks === 1)
    assert(new GenericFeature(chr1_100_200_300_400_plus, Some("name")).numBlocks === 2)
    assert(new GenericFeature(chr1_100_200_300_400_minus, Some("name")).numBlocks === 2)
    assert(new GenericFeature(chr1_1000_1100_1200_1300_1900_2000_plus, Some("name")).numBlocks === 3)
    assert(new Transcript(Block("chr1", 1000, 2000, Plus), Some("name"), Some("gene")).numBlocks === 1)
    assert(new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).numBlocks === 2)
    assert(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene")).numBlocks === 2)
    assert(new Transcript(chr1_1000_1100_1200_1300_1900_2000_plus, Some("name"), Some("gene")).numBlocks === 3)
    assert(MessengerRNA(Block("chr1", 1000, 2000, Plus), 1000, 2000, Some("name"), Some("gene")).numBlocks === 1)
    assert(MessengerRNA(chr1_100_200_300_400_plus, 150, 350, Some("name"), Some("gene")).numBlocks === 2)
    assert(MessengerRNA(chr1_100_200_300_400_minus, 150, 350, Some("name"), Some("gene")).numBlocks === 2)
    assert(MessengerRNA(chr1_1000_1100_1200_1300_1900_2000_plus, 1000, 1250, Some("name"), Some("gene")).numBlocks === 3)
  }

  test("Illegal empty region") {
    intercept[IllegalArgumentException]{ new GenericFeature(Empty, Some("name")) }
    intercept[IllegalArgumentException]{ new Transcript(Empty, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(Empty, 5, 6, Some("name"), Some("gene")) }
  }

}
