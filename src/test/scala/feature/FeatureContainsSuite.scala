package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 10/4/16.
  */
class FeatureContainsSuite extends FunSuite {

  test("Empty contains") {
    assert(!Empty.contains(chr1_5_6_minus), "Empty should not contain block")
    assert(!Empty.contains(chr1_1000_1100_1200_1300_1900_2000_plus), "Empty should not contain block set")
  }

  test("Block contains") {
    assert(!chr2_1000_2000_plus.contains(Empty), "Block should not contain empty")
    assert(chr2_1000_2000_plus.contains(chr2_1000_2000_plus), "Block should contain itself")
    assert(!chr2_1000_2000_plus.contains(Block("chr2", 1100, 1200, Minus, "block")), "Incompatible orientations")
    assert(!chr2_1000_2000_plus.contains(Block("chr2", 1100, 1200, Unstranded, "block")), "Incompatible orientations")
    assert(chr2_1000_2000_plus.contains(Block("chr2", 1100, 1200, Plus, "block")), "Block contains block")
    assert(chr2_1000_2000_plus.contains(Block("chr2", 1100, 1200, Both, "block")), "Block contains block")
    assert(!chr2_1000_2000_plus.contains(Block("chr2", 1100, 2001, Plus, "block")), "Overlapping not contained")
    assert(!chr2_1000_2000_plus.contains(Block("chr2", 999, 1500, Plus, "block")), "Overlapping not contained")
    assert(Block("chr1", 500, 6500, Plus, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Block contains block set")
    assert(!Block("chr2", 500, 6500, Plus, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Different chromosomes")
    assert(Block("chr1", 500, 6500, Both, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Block contains block set")
    assert(!Block("chr1", 500, 600, Plus, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Different spans")
    assert(!Block("chr1", 7000, 8000, Plus, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Different spans")
    assert(!Block("chr1", 2500, 2600, Plus, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Block in intron of BlockSet")
    assert(!Block("chr1", 500, 6500, Unstranded, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Incompatible orientations")
    assert(!Block("chr1", 1500, 6500, Unstranded, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Overlapping not contained")
    assert(!Block("chr1", 500, 5500, Unstranded, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Overlapping not contained")
    assert(!Block("chr1", 3000, 4000, Unstranded, "block").contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Overlapping not contained")
    assert(!Block("chr1", 100, 200, Plus, "").contains(Block("chr2", 100, 200, Plus, "")), "Different chromosomes")
    assert(!Block("chr1", 100, 200, Plus, "").contains(Block("chr1", 200, 300, Plus, "")), "Different spans")
    assert(!Block("chr1", 100, 200, Plus, "").contains(Block("chr1", 50, 60, Plus, "")), "Different spans")
  }

  ignore("BlockSet contains") {
    ???
  }

}
