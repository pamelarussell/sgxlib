package testfeature

import feature._
import org.scalatest.FunSuite
import shared.GTF22Data._

/**
  * Created by prussell on 11/12/16.
  */
class GeneSuite extends FunSuite {

  val chr20_37492471_37492949_Minus = Block("20", 37492471, 37492949, Minus)	 // ENSG00000166619
  val chr20_37492533_37492949_Minus = Block("20", 37492533, 37492949, Minus)	 // ENSG00000166619
  val chr20_37495584_37495692_Minus = Block("20", 37495584, 37495692, Minus)	 // ENSG00000166619
  val chr20_37495584_37495762_Minus = Block("20", 37495584, 37495762, Minus)	 // ENSG00000166619
  val chr20_37499892_37503975_Minus = Block("20", 37499892, 37503975, Minus)	 // ENSG00000166619
  val chr20_37502413_37502554_Minus = Block("20", 37502413, 37502554, Minus)	 // ENSG00000166619
  val chr20_37509103_37509212_Minus = Block("20", 37509103, 37509212, Minus)	 // ENSG00000166619
  val chr20_37509103_37509231_Minus = Block("20", 37509103, 37509231, Minus)	 // ENSG00000166619
  val chr20_37517416_37519350_Minus = Block("20", 37517416, 37519350, Minus)	 // ENSG00000166619
  val chr20_37517417_37519350_Minus = Block("20", 37517417, 37519350, Minus)	 // ENSG00000166619
  val chr20_37518815_37519350_Minus = Block("20", 37518815, 37519350, Minus)	 // ENSG00000166619
  val chr20_37518852_37519350_Minus = Block("20", 37518852, 37519350, Minus)	 // ENSG00000166619
  val chr20_37518892_37519350_Minus = Block("20", 37518892, 37519350, Minus)	 // ENSG00000166619
  val chr20_37518911_37519350_Minus = Block("20", 37518911, 37519350, Minus)	 // ENSG00000166619
  val chr20_37518939_37519350_Minus = Block("20", 37518939, 37519350, Minus)	 // ENSG00000166619
  val chr20_37518988_37519350_Minus = Block("20", 37518988, 37519350, Minus)	 // ENSG00000166619
  val chr20_37519153_37519350_Minus = Block("20", 37519153, 37519350, Minus)	 // ENSG00000166619
  val chr20_37519231_37519350_Minus = Block("20", 37519231, 37519350, Minus)	 // ENSG00000166619
  val chr20_37520235_37520303_Minus = Block("20", 37520235, 37520303, Minus)	 // ENSG00000166619
  val chr20_37520437_37520589_Minus = Block("20", 37520437, 37520589, Minus)	 // ENSG00000166619
  val chr20_37521214_37521403_Plus = Block("20", 37521214, 37521403, Plus)	 // ENSG00000053438
  val chr20_37521218_37521403_Plus = Block("20", 37521218, 37521403, Plus)	 // ENSG00000053438
  val chr20_37521318_37521409_Minus = Block("20", 37521318, 37521409, Minus)	 // ENSG00000166619
  val chr20_37521318_37521451_Minus = Block("20", 37521318, 37521451, Minus)	 // ENSG00000166619
  val chr20_37521318_37521693_Minus = Block("20", 37521318, 37521693, Minus)	 // ENSG00000166619
  val chr20_37522357_37522438_Plus = Block("20", 37522357, 37522438, Plus)	 // ENSG00000053438
  val chr20_37522666_37523690_Plus = Block("20", 37522666, 37523690, Plus)	 // ENSG00000053438
  val chr20_37522666_37523693_Plus = Block("20", 37522666, 37523693, Plus)	 // ENSG00000053438
  val chr20_37524376_37524551_Minus = Block("20", 37524376, 37524551, Minus)	 // ENSG00000166619
  val chr20_37526641_37527060_Minus = Block("20", 37526641, 37527060, Minus)	 // ENSG00000276603
  val chr20_37527573_37527723_Minus = Block("20", 37527573, 37527723, Minus)	 // ENSG00000166619
  val chr20_37527630_37527732_Minus = Block("20", 37527630, 37527732, Minus)	 // ENSG00000166619
  val chr20_37527792_37527862_Minus = Block("20", 37527792, 37527862, Minus)	 // ENSG00000166619
  val chr20_37527792_37527878_Minus = Block("20", 37527792, 37527878, Minus)	 // ENSG00000166619
  val chr20_37527792_37527931_Minus = Block("20", 37527792, 37527931, Minus)	 // ENSG00000166619
  val chr20_37530038_37530543_Plus = Block("20", 37530038, 37530543, Plus)	 // ENSG00000235044
  val chr20_37571102_37571298_Plus = Block("20", 37571102, 37571298, Plus)	 // ENSG00000274469
  val chr20_37571893_37571965_Plus = Block("20", 37571893, 37571965, Plus)	 // ENSG00000274469
  val chr20_37577099_37577211_Plus = Block("20", 37577099, 37577211, Plus)	 // ENSG00000274469
  val chr20_37583866_37583935_Plus = Block("20", 37583866, 37583935, Plus)	 // ENSG00000274469
  val chr20_37602851_37603185_Minus = Block("20", 37602851, 37603185, Minus)	 // ENSG00000228234
  val chr20_37619297_37619439_Minus = Block("20", 37619297, 37619439, Minus)	 // ENSG00000225759
  val chr20_37622090_37622200_Minus = Block("20", 37622090, 37622200, Minus)	 // ENSG00000225759
  val chr20_37622828_37623119_Minus = Block("20", 37622828, 37623119, Minus)	 // ENSG00000225759

  val ENST00000062104 = MessengerRNA(BlockSet(List(
    chr20_37521214_37521403_Plus,
    chr20_37522357_37522438_Plus,
    chr20_37522666_37523693_Plus)),
    37521331, 37522759,
    Some("ENST00000062104"), Some("ENSG00000053438"))

  val ENST00000062104_transcript = new Transcript(BlockSet(List(
    chr20_37521214_37521403_Plus,
    chr20_37522357_37522438_Plus,
    chr20_37522666_37523693_Plus)),
    Some("ENST00000062104"), Some("ENSG00000053438"))

  val ENST00000062104_noGeneId = MessengerRNA(BlockSet(List(
    chr20_37521214_37521403_Plus,
    chr20_37522357_37522438_Plus,
    chr20_37522666_37523693_Plus)),
    37521331, 37522759,
    Some("ENST00000062104"), None)

  val ENST00000411780 = new Transcript(BlockSet(List(
    chr20_37492471_37492949_Minus,
    chr20_37495584_37495692_Minus,
    chr20_37509103_37509231_Minus)),
    Some("ENST00000411780"), Some("ENSG00000166619"))

  val ENST00000414080 = MessengerRNA(BlockSet(List(
    chr20_37518988_37519350_Minus,
    chr20_37521318_37521451_Minus,
    chr20_37527792_37527878_Minus)),
    37518988, 37519174,
    Some("ENST00000414080"), Some("ENSG00000166619"))

  val ENST00000414542 = MessengerRNA(BlockSet(List(
    chr20_37517417_37519350_Minus,
    chr20_37521318_37521451_Minus,
    chr20_37527792_37527931_Minus)),
    37518910, 37519174,
    Some("ENST00000414542"), Some("ENSG00000166619"))

  val ENSG00000166619 = Gene(Set(ENST00000411780, ENST00000414080, ENST00000414542), "ENSG00000166619")

  val gene_different_orientations: Gene = Gene(Set(
    new Transcript(Block("X", 1000, 2000, Plus), None, Some("id")),
    new Transcript(Block("X", 500, 2000, Plus), None, Some("id")),
    new Transcript(Block("X", 1000, 3000, Minus), None, Some("id"))
  ), "id")

  test("Transcripts must have gene ID") {
    intercept[IllegalArgumentException](Gene(Set(ENST00000062104_noGeneId), "ENSG00000053438"))
  }

  test("Gene ID should match that of each transcript") {
    intercept[IllegalArgumentException](Gene(Set(ENST00000062104, ENST00000062104_transcript), "wrong_name"))
  }

  test("Transcripts must be on same chromosome") {
    val t1 = new Transcript(Block("20", 1000, 2000, Plus), Some("transcript"), Some("gene"))
    val t2 = new Transcript(Block("21", 1000, 2000, Plus), Some("transcript"), Some("gene"))
    intercept[IllegalArgumentException](Gene(Set(t1, t2), "gene"))
  }

  test("Get chr") {
    assert(ENSG00000166619.chr === "20")
  }

  test("Gene span") {
    // All same orientation
    assert(ENSG00000166619.span === Block("20", 37492471, 37527931, Minus))
    // Some different orientations
    assert(gene_different_orientations.span === Block("X", 500, 3000, Unstranded))
  }


}
