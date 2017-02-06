package testfeature

import feature.{Block, Minus, Plus, Transcript}
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class TranscriptMiscSuite extends FunSuite {

  test("Illegal orientation") {
    intercept[IllegalArgumentException](new Transcript(chr1_1000_2000_both, Some("name"), Some("gene")))
  }

}
