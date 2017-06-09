package testfeature

import feature.{Block, GenericFeature, Minus, Plus}
import org.scalatest.FunSuite

/**
  * Created by prussell on 6/8/17.
  */
class FeatureDistanceSuite extends FunSuite {

  test("Feature distance") {
    // Different chromosomes
    intercept[IllegalArgumentException](new GenericFeature(chr1_1500_2500_plus, None)
      .distance(new GenericFeature(chr2_1000_2000_plus, None)))
    // Adjacent both directions
    assert(new GenericFeature(chr1_1000_2000_minus, None)
      .distance(new GenericFeature(chr1_2000_2600_3500_3600_3800_4500_plus, None)) === 1)
    assert(new GenericFeature(chr1_2000_3000_3500_3600_4500_4600_plus, None)
      .distance(new GenericFeature(chr1_1000_2000_minus, None)) === 1)
    // Overlap
    assert(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, None)
      .distance(new GenericFeature(chr1_2000_3000_4000_5000_6000_7000_both, None)) === 0)
    assert(new GenericFeature(chr1_500_1500_2000_3500_4500_5500_plus, None)
      .distance(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, None)) === 0)
    // Fully contained different orientation
    assert(new GenericFeature(Block("1", 500, 600, Plus), None)
      .distance(new GenericFeature(Block("1", 400, 700, Minus), None)) === 0)
    assert(new GenericFeature(Block("1", 400, 700, Minus), None)
      .distance(new GenericFeature(Block("1", 500, 600, Plus), None)) === 0)
    // No overlap both directions
    assert(new GenericFeature(chr1_5_6_minus, None)
      .distance(new GenericFeature(chr1_2000_2600_3500_3600_3800_4500_plus, None)) === 1995)
    assert(new GenericFeature(chr1_2000_2600_3500_3600_3800_4500_plus, None)
      .distance(new GenericFeature(chr1_100_200_300_400_minus, None)) === 1601)
  }

}
