package testfeature

import feature._
import org.scalatest.FunSuite
import shared.GTF22Data._

/**
  * Created by prussell on 11/12/16.
  */
class FeatureMiscSuite extends FunSuite {

  test("GenericFeature toBED") {
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")).toBED() ===
      "1\t1000\t2000\tname\t0\t+\t1000\t1000\t0,0,0\t1\t1000\t0")
    assert(new GenericFeature(chr1_1000_2000_both, Some("name")).toBED(score = 5, thickStartEnd = Some((1500,1600))) ===
      "1\t1000\t2000\tname\t5\t.\t1500\t1600\t0,0,0\t1\t1000\t0")
    assertThrows[IllegalArgumentException](
      new GenericFeature(chr1_1000_2000_plus_1, Some("name")).toBED(score = 5, thickStartEnd = Some((1500,2600)))
    )
   assertThrows[IllegalArgumentException](
      new GenericFeature(chr1_1000_2000_plus_1, Some("name")).toBED(rgb = (256,0,0))
    )
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")).toBED(rgb = (0,0,255)) ===
      "1\t1000\t2000\tname\t0\t+\t1000\t1000\t0,0,255\t1\t1000\t0")
    assert(new GenericFeature(chr1_1000_2000_plus_1, None).toBED(rgb = (0,0,255), score = 5) ===
      "1\t1000\t2000\t.\t5\t+\t1000\t1000\t0,0,255\t1\t1000\t0")
    assert(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_both, None).toBED() ===
      "1\t1000\t6000\t.\t0\t.\t1000\t1000\t0,0,0\t3\t1000,1000,1000\t0,2000,4000")
  }

  test("Transcript toBED") {
    assert(new Transcript(chr1_1000_2000_3000_4000_5000_6000_plus, None, None).toBED() ===
      "1\t1000\t6000\t.\t0\t+\t1000\t1000\t0,0,0\t3\t1000,1000,1000\t0,2000,4000")
  }

  test("Messenger RNA toBED") {
    assert(transcript1400001.toBED() ===
      "140\t65148\t73504\t140.000.1\t0\t-\t66992\t71807\t0,0,0\t5\t339,177,88,112,282\t0,1674,5058,6547,8074")
  }

  test("GenericFeature equals and hashCode") {
    // Equals
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) === new GenericFeature(chr1_1000_2000_plus_2, Some("name")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) === new GenericFeature(chr1_1000_2000_plus_2, None))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) === new GenericFeature(chr1_1000_2000_plus_1, None))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) != new GenericFeature(chr1_1000_2000_plus_1, Some("name")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) != new GenericFeature(chr1_1500_2500_minus, Some("name")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) equals new GenericFeature(chr1_1000_2000_plus_2, Some("name")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) !=
      new Transcript(chr1_1000_2000_plus_2, Some("name"), Some("gene")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) equals new GenericFeature(chr1_1000_2000_plus_2, None))
    assert(new GenericFeature(chr1_1000_2000_plus_1, None) equals new GenericFeature(chr1_1000_2000_plus_1, None))
    assert(!(new GenericFeature(chr1_1000_2000_plus_1, None) equals new GenericFeature(chr1_1000_2000_plus_1, Some("name"))))
    assert(!(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) equals new GenericFeature(chr1_1500_2500_minus, Some("name"))))
    assert(!(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) equals new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene"))))
    assert(!(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) equals MessengerRNA(chr1_1000_2000_plus_1, 1200, 1212, Some("name"), Some("gene"))))
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) != "random string")
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
    assert(new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")) != MessengerRNA(chr1_1000_2000_plus_1, 1000, 1006, Some("name"), Some("gene")))
    assert(new GenericFeature(chr1_1000_2000_plus_1, Some("name")) != MessengerRNA(chr1_1000_2000_plus_1, 1000, 1006, Some("name"), Some("gene")))
    assert(new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("gene")) != MessengerRNA(chr1_1000_2000_plus_1, 1000, 1006, Some("name"), Some("gene")))
  }

  test("Messenger RNA equals and hashCode") {
    val mr1 = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1599, Some("name"), Some("gene"))
    val mr2 = MessengerRNA(chr1_1000_2000_plus_2, 1500, 1599, Some("name"), Some("gene"))
    val mr3 = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1740, Some("name"), Some("gene"))
    val mr4 = MessengerRNA(chr1_1000_2000_plus_1, 1400, 1640, Some("name"), Some("gene"))
    val mr5 = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1599, Some("name2"), Some("gene"))
    val mr6 = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1599, Some("name"), Some("gene2"))
    val mr7 = MessengerRNA(chr1_900_2100_3000_4000_plus, 1500, 1599, Some("name"), Some("gene"))
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
    val mr = MessengerRNA(chr1_1000_2000_plus_1, 1500, 1599, Some("name"), Some("gene"))
    val mr2 = MessengerRNA(chr1_1000_2000_plus_1, 1000, 1300, Some("name"), Some("gene"))
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
    assert(!gf.equals(tr))
    assert(!gf.equals(mr))
    assert(!gf.equals(mr2))
    assert(!tr.equals(gf))
    assert(!tr.equals(mr))
    assert(!tr.equals(mr2))
    assert(!mr.equals(gf))
    assert(!mr2.equals(gf))
    assert(!mr.equals(tr))
    assert(!mr2.equals(tr))
    assert(!mr.equals(mr2))
    assert(!mr2.equals(mr))
  }

  test("Transcript and mRNA valid orientation") {
    val b3 = Block("1", 1000, 2000, Unstranded)
    val b4 = Block("1", 3000, 4000, Unstranded)
    val bs2 = BlockSet(List(b3, b4))
    intercept[IllegalArgumentException]{ new Transcript(b3, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ new Transcript(bs2, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(b3, 1500, 1599, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(bs2, 1500, 1599, Some("name"), Some("gene")) }
  }

  test("Num blocks") {
    assert(new GenericFeature(Block("1", 1000, 2000, Plus), Some("name")).numBlocks === 1)
    assert(new GenericFeature(chr1_100_200_300_400_plus, Some("name")).numBlocks === 2)
    assert(new GenericFeature(chr1_100_200_300_400_minus, Some("name")).numBlocks === 2)
    assert(new GenericFeature(chr1_1000_1100_1200_1300_1900_2000_plus, Some("name")).numBlocks === 3)
    assert(new Transcript(Block("1", 1000, 2000, Plus), Some("name"), Some("gene")).numBlocks === 1)
    assert(new Transcript(chr1_100_200_300_400_plus, Some("name"), Some("gene")).numBlocks === 2)
    assert(new Transcript(chr1_100_200_300_400_minus, Some("name"), Some("gene")).numBlocks === 2)
    assert(new Transcript(chr1_1000_1100_1200_1300_1900_2000_plus, Some("name"), Some("gene")).numBlocks === 3)
    assert(MessengerRNA(Block("1", 1000, 2000, Plus), 1000, 1099, Some("name"), Some("gene")).numBlocks === 1)
    assert(MessengerRNA(chr1_100_200_300_400_plus, 150, 180, Some("name"), Some("gene")).numBlocks === 2)
    assert(MessengerRNA(chr1_100_200_300_400_minus, 150, 180, Some("name"), Some("gene")).numBlocks === 2)
    assert(MessengerRNA(chr1_1000_1100_1200_1300_1900_2000_plus, 1000, 1220, Some("name"), Some("gene")).numBlocks === 3)
  }

  test("Illegal empty names") {
    intercept[IllegalArgumentException](new GenericFeature(chr1_1000_2000_plus_1, Some("")))
    intercept[IllegalArgumentException](new Transcript(chr1_1000_2000_plus_1, Some(""), Some("gene")))
    intercept[IllegalArgumentException](new Transcript(chr1_1000_2000_plus_1, Some("name"), Some("")))
    intercept[IllegalArgumentException](MessengerRNA(chr1_1000_2000_plus_1, 1000, 1012, Some(""), Some("gene")))
    intercept[IllegalArgumentException](MessengerRNA(chr1_1000_2000_plus_1, 1000, 1012, Some("name"), Some("")))
  }

  test("Illegal empty region") {
    intercept[IllegalArgumentException]{ new GenericFeature(Empty, Some("name")) }
    intercept[IllegalArgumentException]{ new Transcript(Empty, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(Empty, 50, 62, Some("name"), Some("gene")) }
  }

  test("toString") {
    assert(new GenericFeature(chr1_1900_2000_3000_4000_plus, Some("name")).toString ===
      "(feature.GenericFeature[[1:1900-2000:+], [1:3000-4000:+]],Some(name))")
    assert(new Transcript(chr1_1900_2000_3000_4000_plus, Some("name"), Some("gene")).toString ===
      "(feature.Transcript[[1:1900-2000:+], [1:3000-4000:+]],Some(name),Some(gene))")
    assert(MessengerRNA(chr1_1900_2000_3000_4000_plus, 1900, 1906, Some("name"), Some("gene")).toString ===
      "(feature.MessengerRNA[[1:1900-2000:+], [1:3000-4000:+]],Some(name),Some(gene),CDS:1900-1906)")
  }

}
