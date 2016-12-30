package testcollection

import collection.{FeatureSet, GTF22FeatureSet}
import feature._
import org.scalatest.FunSuite
import shared.GTF22Data._

/**
  * Created by prussell on 12/28/16.
  */
class FeatureSetSuite extends FunSuite {

  val fewFeatures: FeatureSet[Feature] = new GTF22FeatureSet("resources/sample_genes.gtf")
  val threeChrs: FeatureSet[Feature] = new GTF22FeatureSet("resources/Homo_sapiens.GRCh38.86.chr_20_21_22.gtf")

  test("Three chromosomes") {
    assert(threeChrs.size === 11127)
  }

  test("Few features") {
    assert(!fewFeatures.contains(new GenericFeature(Block("chr2", 100, 200, Plus), None)))
    assert(!fewFeatures.contains(new GenericFeature(Block("1", 963551, 964164, Plus), None)))
    assert(!fewFeatures.contains(new GenericFeature(Block("1", 963551, 1000000, Plus), None)))
    assert(!fewFeatures.contains(new GenericFeature(Block("1", 963551, 964164, Minus), None)))
    assert(fewFeatures.contains(ENST00000338591))
    assert(fewFeatures.contains(ENST00000412115))
    assert(fewFeatures.contains(ENST00000427857))
    assert(fewFeatures.contains(ENST00000429505))
    assert(fewFeatures.contains(ENST00000432963))
    assert(fewFeatures.contains(ENST00000434264))
    assert(fewFeatures.contains(ENST00000443772))
    assert(fewFeatures.contains(ENST00000446136))
    assert(fewFeatures.contains(ENST00000457084))
    assert(fewFeatures.contains(ENST00000463212))
    assert(fewFeatures.contains(ENST00000473798))
    assert(fewFeatures.contains(ENST00000481067))
    assert(fewFeatures.contains(ENST00000585745))
    assert(fewFeatures.contains(ENST00000585768))
    assert(fewFeatures.contains(ENST00000585826))
    assert(fewFeatures.contains(ENST00000586288))
    assert(fewFeatures.contains(ENST00000586928))
    assert(fewFeatures.contains(ENST00000587530))
    assert(fewFeatures.contains(ENST00000588951))
    assert(fewFeatures.contains(ENST00000589531))
    assert(fewFeatures.contains(ENST00000589899))
    assert(fewFeatures.contains(ENST00000590848))
    assert(fewFeatures.contains(ENST00000591440))
    assert(fewFeatures.contains(ENST00000591702))
    assert(fewFeatures.contains(ENST00000592547))
    assert(fewFeatures.contains(ENST00000593022))
    assert(fewFeatures.contains(ENST00000609830))
    assert(fewFeatures.contains(ENST00000622660))
    assert(fewFeatures.contains(ENST00000623083))
    assert(fewFeatures.contains(ENST00000623834))
    assert(fewFeatures.contains(ENST00000624735))
    assert(fewFeatures.contains(ENST00000466300))
    assert(fewFeatures.contains(inter_140_5140_8522_Minus))
    assert(fewFeatures.contains(inter_CNS_140_8522_9711_Minus))
    assert(fewFeatures.contains(inter_140_9711_13182_Minus))
    assert(fewFeatures.contains(intron_CNS_140_70102_70151_Minus))
    assert(fewFeatures.contains(transcript1400001))
    assert(fewFeatures.size === 37)
  }

}
