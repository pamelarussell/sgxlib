package testreader

import feature.{BlockSet, Feature, MessengerRNA}
import org.scalatest.FunSuite
import reader.GTF2Reader

/**
  * Created by prussell on 12/18/16.
  */
class AnnotFileReaderSuite extends FunSuite {

  val features: Set[Feature] = GTF2Reader.load(gtfFiveGenes.split("\n").iterator)

  test("Five genes") {
    assert(features.contains(ENST00000338591))
    assert(features.contains(ENST00000412115))
    assert(features.contains(ENST00000427857))
    assert(features.contains(ENST00000429505))
    assert(features.contains(ENST00000432963))
    assert(features.contains(ENST00000434264))
    assert(features.contains(ENST00000443772))
    assert(features.contains(ENST00000446136))
    assert(features.contains(ENST00000457084))
    assert(features.contains(ENST00000463212))
    assert(features.contains(ENST00000473798))
    assert(features.contains(ENST00000481067))
    assert(features.contains(ENST00000585745))
    assert(features.contains(ENST00000585768))
    assert(features.contains(ENST00000585826))
    assert(features.contains(ENST00000586288))
    assert(features.contains(ENST00000586928))
    assert(features.contains(ENST00000587530))
    assert(features.contains(ENST00000588951))
    assert(features.contains(ENST00000589531))
    assert(features.contains(ENST00000589899))
    assert(features.contains(ENST00000590848))
    assert(features.contains(ENST00000591440))
    assert(features.contains(ENST00000591702))
    assert(features.contains(ENST00000592547))
    assert(features.contains(ENST00000593022))
    assert(features.contains(ENST00000609830))
    assert(features.contains(ENST00000622660))
    assert(features.contains(ENST00000623083))
    assert(features.contains(ENST00000623834))
    assert(features.contains(ENST00000624735))
    assert(features.size === 31)
  }

}
