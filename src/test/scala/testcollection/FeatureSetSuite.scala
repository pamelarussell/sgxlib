package testcollection

import java.io.File

import collection.{FeatureSet, GTF22FeatureSet}
import feature._
import org.scalatest.FunSuite
import shared.GTF22Data._

/**
  * Created by prussell on 12/28/16.
  */
class FeatureSetSuite extends FunSuite {

  val fewFeatures: FeatureSet[Feature] =
    new GTF22FeatureSet(new File(getClass.getResource("/sample_genes.gtf").getPath))

  test("Iterator") {
    val iter1 = fewFeatures.iterator
    assert(iter1.size == 37)
    val iter2 = fewFeatures.iterator
    assert(iter2.exists(feat => feat.name.contains("ENST00000432963")))
    val iter3 = fewFeatures.iterator
    assert(iter3.exists(feat => feat.name.contains("140.000.1")))
    val iter4 = fewFeatures.iterator
    assert(iter4.exists(feat => feat.name.contains("intron_CNS_140_70102_70151_140.000.1")))
    val iter5 = fewFeatures.iterator
    assert(iter5.contains(inter_140_9711_13182_Minus))
    val iter6 = fewFeatures.iterator
    assert(iter6.contains(inter_140_5140_8522_Minus))
  }

  test("Three chromosomes") {
    assert(chr20_21_22.contains(ENST00000411780))
    assert(chr20_21_22.contains(ENST00000447625))
    assert(chr20_21_22.contains(ENST00000613961))
    assert(chr20_21_22.contains(ENST00000467603))
    assert(chr20_21_22.contains(ENST00000397137))
    assert(chr20_21_22.contains(ENST00000373537))
    assert(chr20_21_22.contains(ENST00000414542))
    assert(chr20_21_22.contains(ENST00000397135))
    assert(chr20_21_22.contains(ENST00000397134))
    assert(chr20_21_22.contains(ENST00000397131))
    assert(chr20_21_22.contains(ENST00000432507_transcript))
    assert(chr20_21_22.contains(ENST00000445723_transcript))
    assert(chr20_21_22.contains(ENST00000414080))
    assert(chr20_21_22.contains(ENST00000456058))
    assert(chr20_21_22.contains(ENST00000062104))
    assert(!chr20_21_22.contains(ENST00000062104_generic))
    assert(!chr20_21_22.contains(ENST00000062104_transcript))
    assert(chr20_21_22.contains(ENST00000346199))
    assert(chr20_21_22.contains(ENST00000620327))
    assert(chr20_21_22.contains(ENST00000450588))
    assert(chr20_21_22.contains(ENST00000621390))
    assert(chr20_21_22.contains(ENST00000423261))
    assert(chr20_21_22.contains(ENST00000457816))
    assert(chr20_21_22.size === 11127)
  }

  test("Interval overlap") {

    val plusOverlap = chr20_21_22.overlappers("chr20", 37476591, 37645612, Plus).toSet
    val minusOverlap = chr20_21_22.overlappers("20", 37476591, 37645612, Minus).toSet
    val bothOverlap = chr20_21_22.overlappers("chr20", 37476591, 37645612, Unstranded).toSet

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

  }

  test("Empty interval overlap") {

    // Wrong orientation
    assert(chr20_21_22.overlappers("chr20", 37564882, 37590261, Minus).isEmpty)

    // In intron
    assert(chr20_21_22.overlappers("20", 37579497, 37581584, Plus).isEmpty)

    // No genes
    assert(chr20_21_22.overlappers("chr20", 37584686, 37586304, Plus).isEmpty)

    // Nonexistent chromosome
    assert(chr20_21_22.overlappers("fakeChr", 100, 200, Plus).isEmpty)
    assert(chr20_21_22.overlappers(new GenericFeature(Block("fakeChr", 100, 200, Plus), None)).isEmpty)

  }

  test("Feature overlap") {

    val featOverlap1 = chr20_21_22.overlappers(ENST00000613961).toSet
    assert(featOverlap1.size === 2)
    assert(featOverlap1.contains(ENST00000613961))
    assert(featOverlap1.contains(ENST00000467603))

    val featOverlap2 = chr20_21_22.overlappers(ENST00000414542).toSet
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
    assert(chr20_21_22.overlappers(new GenericFeature(Block("20", 37564882, 37590261, Minus), None)).isEmpty)

    // In intron
    assert(chr20_21_22.overlappers(new GenericFeature(Block("20", 37579497, 37581584, Plus), None)).isEmpty)

    // No genes
    assert(chr20_21_22.overlappers(new GenericFeature(Block("20", 37584686, 37586304, Plus), None)).isEmpty)

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

  test("Nearest feature") {
    // Overlap multiple features - single point
    val set1 = chr20_21_22.nearest("chr20", 37306985, 37306986).toSet
    assert(set1.size === 5)
    assert(set1.contains(ENST00000373606))
    assert(set1.contains(ENST00000397150))
    assert(set1.contains(ENST00000397152))
    assert(set1.contains(ENST00000373605))
    assert(set1.contains(ENST00000397151))
    // Overlap multiple features
    val set2 = chr20_21_22.nearest("chr20", 37306985, 37307161).toSet
    assert(set2 === set1)
   // Overlap multiple features - no overlapping blocks
    val feat1 = new GenericFeature(BlockSet(List(
      Block("20", 37306985, 37306986, Plus),
      Block("20", 37313141, 37314129, Plus))), None)
    val set3 = chr20_21_22.nearest(feat1).toSet
    assert(set3 === set1)
    val feat2 = new GenericFeature(BlockSet(List(
      Block("20", 37306985, 37306986, Plus),
      Block("20", 37319231, 37319657, Plus))), None)
    val set4 = chr20_21_22.nearest(feat2).toSet
    assert(set4 === set1)
    val feat3 = new GenericFeature(BlockSet(List(
      Block("20", 37282313, 37285909, Plus),
      Block("20", 37313141, 37314129, Plus))), None)
    val set5 = chr20_21_22.nearest(feat3).toSet
    assert(set5 === set1)
    // Overlap some of each orientation
    val feat4 = new GenericFeature(Block("20", 37178077, 37179086, Plus), None)
    val set6 = chr20_21_22.nearest(feat4).toSet
    assert(set6.size === 8)
    assert(set6.contains(ENST00000237530))
    assert(set6.contains(ENST00000456102))
    assert(set6.contains(ENST00000400441))
    // No overlap
    val feat6 = new GenericFeature(Block("20", 37249854, 37250291, Minus), None)
    val set8 = chr20_21_22.nearest(feat6).toSet
    assert(set8.size === 1)
    assert(set8.contains(ENST00000373614))
    // No overlap, several neighbors same distance
    val feat7 = new GenericFeature(Block("20", 38954507, 38954508, Unstranded), None)
    val set9 = chr20_21_22.nearest(feat7).toSet
    assert(set9.size === 3)
    assert(set9.contains(ENST00000619304))
    assert(set9.contains(ENST00000619850))
    assert(set9.contains(ENST00000620080))
    // No features on chromosome
    val iter2 = chr20_21_22.nearest("chr200", 37306985, 37307161)
    assert(iter2.isEmpty)
    // Overlapping two non-overlapping features
    val feat8 = new GenericFeature(Block("20", 38953099, 38955928, Plus), None)
    val set10 = chr20_21_22.nearest(feat8).toSet
    assert(set10 === set9)
    // Before first feature on chromosome
    val feat9 = new GenericFeature(Block("20", 1000, 2000, Minus), None)
    val set11 = chr20_21_22.nearest(feat9).toSet
    assert(set11.size === 1)
    assert(set11.contains(ENST00000608838))
    // After last feature on chromosome
    val feat12 = new GenericFeature(Block("20", 65327972, 66327972, Plus), None)
    val set14 = chr20_21_22.nearest(feat12).toSet
    assert(set14.size === 1)
    assert(set14.contains(ENST00000620521))
  }

}
