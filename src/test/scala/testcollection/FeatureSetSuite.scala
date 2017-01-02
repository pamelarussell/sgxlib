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
    assert(threeChrs.contains(ENST00000411780))
    assert(threeChrs.contains(ENST00000447625))
    assert(threeChrs.contains(ENST00000613961))
    assert(threeChrs.contains(ENST00000467603))
    assert(threeChrs.contains(ENST00000397137))
    assert(threeChrs.contains(ENST00000373537))
    assert(threeChrs.contains(ENST00000414542))
    assert(threeChrs.contains(ENST00000397135))
    assert(threeChrs.contains(ENST00000397134))
    assert(threeChrs.contains(ENST00000397131))
    assert(threeChrs.contains(ENST00000432507_transcript))
    assert(threeChrs.contains(ENST00000445723_transcript))
    assert(threeChrs.contains(ENST00000414080))
    assert(threeChrs.contains(ENST00000456058))
    assert(threeChrs.contains(ENST00000062104))
    assert(!threeChrs.contains(ENST00000062104_generic))
    assert(!threeChrs.contains(ENST00000062104_transcript))
    assert(threeChrs.contains(ENST00000346199))
    assert(threeChrs.contains(ENST00000620327))
    assert(threeChrs.contains(ENST00000450588))
    assert(threeChrs.contains(ENST00000621390))
    assert(threeChrs.contains(ENST00000423261))
    assert(threeChrs.contains(ENST00000457816))
    assert(threeChrs.size === 11127)
  }

  test("Interval overlap") {

    val plusOverlap = threeChrs.overlappers("chr20", 37476591, 37645612, Plus).toSet
    val minusOverlap = threeChrs.overlappers("20", 37476591, 37645612, Minus).toSet
    val bothOverlap = threeChrs.overlappers("chr20", 37476591, 37645612, Both).toSet
    val unstrandedOverlap = threeChrs.overlappers("20", 37476591, 37645612, Unstranded).toSet

    assert(bothOverlap.size === 21)
    assert(bothOverlap.contains(ENST00000411780))
    assert(bothOverlap.contains(ENST00000447625))
    assert(bothOverlap.contains(ENST00000613961))
    assert(bothOverlap.contains(ENST00000467603))
    assert(bothOverlap.contains(ENST00000397137))
    assert(bothOverlap.contains(ENST00000373537))
    assert(bothOverlap.contains(ENST00000414542))
    assert(bothOverlap.contains(ENST00000397135))
    assert(bothOverlap.contains(ENST00000397134))
    assert(bothOverlap.contains(ENST00000397131))
    assert(bothOverlap.contains(ENST00000432507_transcript))
    assert(bothOverlap.contains(ENST00000445723_transcript))
    assert(bothOverlap.contains(ENST00000414080))
    assert(bothOverlap.contains(ENST00000456058))
    assert(bothOverlap.contains(ENST00000062104))
    assert(!bothOverlap.contains(ENST00000062104_generic))
    assert(!bothOverlap.contains(ENST00000062104_transcript))
    assert(bothOverlap.contains(ENST00000346199))
    assert(bothOverlap.contains(ENST00000620327))
    assert(bothOverlap.contains(ENST00000450588))
    assert(bothOverlap.contains(ENST00000621390))
    assert(bothOverlap.contains(ENST00000423261))
    assert(bothOverlap.contains(ENST00000457816))

    assert(plusOverlap.size === 4)
    assert(plusOverlap.contains(ENST00000062104))
    assert(plusOverlap.contains(ENST00000346199))
    assert(plusOverlap.contains(ENST00000450588))
    assert(plusOverlap.contains(ENST00000621390))

    assert(minusOverlap.size === 17)
    assert(minusOverlap.contains(ENST00000411780))
    assert(minusOverlap.contains(ENST00000447625))
    assert(minusOverlap.contains(ENST00000613961))
    assert(minusOverlap.contains(ENST00000467603))
    assert(minusOverlap.contains(ENST00000397137))
    assert(minusOverlap.contains(ENST00000373537))
    assert(minusOverlap.contains(ENST00000414542))
    assert(minusOverlap.contains(ENST00000397135))
    assert(minusOverlap.contains(ENST00000397134))
    assert(minusOverlap.contains(ENST00000397131))
    assert(minusOverlap.contains(ENST00000432507_transcript))
    assert(minusOverlap.contains(ENST00000445723_transcript))
    assert(minusOverlap.contains(ENST00000414080))
    assert(minusOverlap.contains(ENST00000456058))
    assert(!minusOverlap.contains(ENST00000062104_generic))
    assert(!minusOverlap.contains(ENST00000062104_transcript))
    assert(minusOverlap.contains(ENST00000620327))
    assert(minusOverlap.contains(ENST00000423261))
    assert(minusOverlap.contains(ENST00000457816))

    assert(unstrandedOverlap.size === 0)

  }

  test("Empty interval overlap") {

    // Wrong orientation
    assert(threeChrs.overlappers("chr20", 37564882, 37590261, Minus).isEmpty)

    // In intron
    assert(threeChrs.overlappers("20", 37579497, 37581584, Plus).isEmpty)

    // No genes
    assert(threeChrs.overlappers("chr20", 37584686, 37586304, Plus).isEmpty)

  }

  test("Feature overlap") {

    val featOverlap1 = threeChrs.overlappers(ENST00000613961).toSet
    assert(featOverlap1.size === 2)
    assert(featOverlap1.contains(ENST00000613961))
    assert(featOverlap1.contains(ENST00000467603))

    val featOverlap2 = threeChrs.overlappers(ENST00000414542).toSet
    assert(featOverlap2.size === 11)
    assert(featOverlap2.contains(ENST00000373537))
    assert(featOverlap2.contains(ENST00000397137))
    assert(featOverlap2.contains(ENST00000467603))
    assert(featOverlap2.contains(ENST00000414542))
    assert(featOverlap2.contains(ENST00000397135))
    assert(featOverlap2.contains(ENST00000397134))
    assert(featOverlap2.contains(ENST00000397131))
    assert(featOverlap2.contains(ENST00000432507_transcript))
    assert(featOverlap2.contains(ENST00000445723_transcript))
    assert(featOverlap2.contains(ENST00000414080))
    assert(featOverlap2.contains(ENST00000456058))

  }

  test("Empty feature overlap") {

    // Wrong orientation
    assert(threeChrs.overlappers(new GenericFeature(Block("20", 37564882, 37590261, Minus), None)).isEmpty)

    // In intron
    assert(threeChrs.overlappers(new GenericFeature(Block("20", 37579497, 37581584, Plus), None)).isEmpty)

    // No genes
    assert(threeChrs.overlappers(new GenericFeature(Block("20", 37584686, 37586304, Plus), None)).isEmpty)

  }

  test("Few features") {
    assert(!fewFeatures.contains(new GenericFeature(Block("2", 100, 200, Plus), None)))
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
