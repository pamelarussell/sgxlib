package testreader

import feature.Feature
import org.scalatest.FunSuite
import reader._

/**
  * Created by prussell on 12/18/16.
  */
class AnnotFileReaderSuite extends FunSuite {

  val fewFeatures: Set[Feature] = GTF22Reader.load(gtfLines.split("\n").iterator)
  val threeChrs: Set[Feature] = GTF22Reader.load("resources/Homo_sapiens.GRCh38.86.chr_20_21_22.gtf")

  test("Three chromosomes") {
    assert(threeChrs.size === 11127)
  }

  test("Few features") {
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
