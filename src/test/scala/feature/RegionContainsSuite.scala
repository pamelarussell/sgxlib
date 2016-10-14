package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 10/4/16.
  */
class RegionContainsSuite extends FunSuite {

  test("Empty contains") {
    assert(!Empty.contains(chr1_5_6_minus), "Empty should not contain block")
    assert(!Empty.contains(chr1_1000_1100_1200_1300_1900_2000_plus), "Empty should not contain block set")
  }

  test("Block contains") {
    assert(!chr2_1000_2000_plus.contains(Empty), "Block should not contain empty")
    assert(chr2_1000_2000_plus.contains(chr2_1000_2000_plus), "Block should contain itself")
    assert(!chr2_1000_2000_plus.contains(Block("chr2", 1100, 1200, Minus)), "Incompatible orientations")
    assert(!chr2_1000_2000_plus.contains(Block("chr2", 1100, 1200, Unstranded)), "Incompatible orientations")
    assert(chr2_1000_2000_plus.contains(Block("chr2", 1100, 1200, Plus)), "Block contains block")
    assert(chr2_1000_2000_plus.contains(Block("chr2", 1100, 1200, Both)), "Block contains block")
    assert(!chr2_1000_2000_plus.contains(Block("chr2", 1100, 2001, Plus)), "Overlapping not contained")
    assert(!chr2_1000_2000_plus.contains(Block("chr2", 999, 1500, Plus)), "Overlapping not contained")
    assert(Block("chr1", 500, 6500, Plus).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Block contains block set")
    assert(!Block("chr2", 500, 6500, Plus).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Different chromosomes")
    assert(Block("chr1", 500, 6500, Both).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Block contains block set")
    assert(!Block("chr1", 500, 600, Plus).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Different spans")
    assert(!Block("chr1", 7000, 8000, Plus).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Different spans")
    assert(!Block("chr1", 2500, 2600, Plus).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Block in intron of BlockSet")
    assert(!Block("chr1", 500, 6500, Unstranded).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Incompatible orientations")
    assert(!Block("chr1", 1500, 6500, Unstranded).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Overlapping not contained")
    assert(!Block("chr1", 500, 5500, Unstranded).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Overlapping not contained")
    assert(!Block("chr1", 3000, 4000, Unstranded).contains(chr1_1000_2000_3000_4000_5000_6000_plus),
      "Overlapping not contained")
    assert(!Block("chr1", 100, 200, Plus).contains(Block("chr2", 100, 200, Plus)), "Different chromosomes")
    assert(!Block("chr1", 100, 200, Plus).contains(Block("chr1", 200, 300, Plus)), "Different spans")
    assert(!Block("chr1", 100, 200, Plus).contains(Block("chr1", 50, 60, Plus)), "Different spans")
  }

  test("BlockSet contains") {

    /*
    Block set contains block
     */

    // Strictly nested both directions
    assert(chr1_900_2100_3000_4000_plus.contains(chr1_1000_2000_both), "One block of block set completely contains the other block")
    assert(!chr1_1100_1200_1300_1400_plus.contains(chr1_1000_2000_both), "Block completely contains block set")
    // Block set nested inside block sharing one endpoint
    assert(!chr1_1000_1100_1200_1300_plus.contains(chr1_1000_2000_both), "Block completely contains block set, sharing one endpoint")
    // Block nested inside block set sharing one endpoint
    assert(chr1_1000_1100_1200_1300_1900_2000_plus.contains(Block("chr1", 1250, 1300, Plus)), "Block nested inside block set sharing one endpoint")
    // Overlapping one block off end
    assert(!chr1_1900_2100_3000_4000_plus.contains(chr1_1000_2000_both), "Overlapping one block off end")
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 2000, 2150, Plus)), "Overlapping one block off end")
    // Overlapping two blocks off end
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 1550, 1850, Plus)), "Overlapping two blocks off end")
    // Overlapping one block and intron
    assert(!chr1_1500_1600_2100_2300_plus.contains(chr1_1000_2000_both), "Overlapping one block and an intron")
    // Overlapping two blocks and intron
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(chr1_1000_2000_both), "Overlapping two blocks and an intron")
    // Overlapping three blocks and intron
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.contains(Block("chr1", 500, 6500, Plus)), "Overlapping three blocks and an intron")
    // Same span as one block
    assert(chr1_1000_2000_3000_4000_plus.contains(chr1_1000_2000_both), "Same span as one block")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 1700, 1800, Plus)), "Same span as one block")
    // Contains one block sharing an endpoint
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 1650, 1800, Plus)), "Contains one block sharing an endpoint")
    // Non-overlapping because different spans
    assert(!chr1_100_200_300_400_plus.contains(chr1_1000_2000_plus_1), "Non-overlapping because different spans")
    assert(!chr1_100_200_300_400_plus.contains(Block("chr1", 500, 600, Both)), "Non-overlapping because different spans")
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 1000, 1100, Both)), "Non-overlapping because different spans")
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 2400, 2500, Both)), "Non-overlapping because different spans")
    // Non-overlapping because different orientations
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(chr1_1000_2000_minus), "Non-overlapping because different orientations")
    // Different chromosomes
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.contains(chr2_1000_2000_plus), "Different chromosomes")
    // Same span as an intron
    // Two blocks
    assert(!chr1_100_200_300_400_plus.contains(Block("chr1", 200, 300, Plus)), "Same span as an intron")
    // Three blocks
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 1800, 2100, Plus)), "Same span as an intron")
    // Adjacent to span
    // Two blocks
    assert(!chr1_100_200_300_400_plus.contains(Block("chr1", 400, 500, Plus)), "Adjacent to span")
    // Three blocks
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 1400, 1500, Plus)), "Same span as an intron")
    // 1bp gap between blocks
    // In intron
    assert(!chr1_100_200_300_400_plus.contains(Block("chr1", 201, 299, Plus)), "1bp gap between blocks")
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 1601, 1700, Plus)), "1bp gap between blocks")
    // Outside span
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 1400, 1499, Plus)), "1bp gap between blocks")
    assert(!chr1_1500_1600_1700_1800_2100_2300_plus.contains(Block("chr1", 2300, 2400, Plus)), "1bp gap between blocks")

    /*
    Block set contains block set
     */

    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_500_600_2500_2600_5500_5600_plus), "One block nested, others non-overlapping")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_2000_2600_3500_3600_6500_6600_plus), "One block nested, others non-overlapping")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_500_600_700_800_900_1100_both), "One block overlapping off end of span")
    assert(!chr1_1000_2000_3000_4000_plus.contains(chr1_500_600_900_2000_both), "One block overlapping off end of span")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_3000_4000_4500_5500_plus), "One block overlapping one block of other")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_500_1500_3500_3600_7000_8000_plus), "One block overlapping one block, another block nested in another block")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus), "One block overlapping one block, another block nested in another block")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_500_6500_7000_8000_plus), "One block overlapping three blocks of other")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_2500_8500_9000_10000_plus), "One block overlapping three blocks of other")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_2000_8500_9000_10000_plus), "One block overlapping three blocks of other")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_2500_3500_5500_5600_plus), "One block overlapping two blocks of other")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_2500_5000_5500_5600_plus), "One block overlapping two blocks of other")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_500_600_1000_2000_plus), "One block same")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_5000_6000_7000_8000_plus), "One block same")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_2500_3000_4500_4600_5000_6000_plus), "One block same")
    assert(chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_1000_2000_3000_4000_5000_6000_plus), "Same blocks compatible orientation")
    assert(!chr1_1000_2000_3000_4000_5000_6000_plus.contains(chr1_7000_8000_9000_10000_minus), "Same chromosome, different orientation, non-overlapping spans")
    assert(!chr1_1000_2000_3000_4000_5000_6000_plus.contains(chr1_1000_4000_7000_8000_minus), "Same chromosome, different orientation, overlapping spans")
    assert(!chr1_1000_2000_3000_4000_5000_6000_plus.contains(chr1_6000_7000_8000_9000_plus), "Same chromosome, same orientation, adjacent spans")
    assert(!chr1_1000_2000_3000_4000_5000_6000_plus.contains(chr1_2000_3000_4000_5000_plus), "Same chromosome, same orientation, interleaved exons completely filling span")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_2000_3000_4000_5000_6000_7000_both), "Same chromosome, same orientation, interleaved exons completely filling span")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_2000_3000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_2000_3000_4000_5000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_900_1000_4000_5000_6000_7000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_2500_2600_4500_4600_6500_6600_both), "Same chromosome, same orientation, interleaved exons none adjacent")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_4500_4600_6500_6600_8500_8600_both), "Same chromosome, same orientation, interleaved exons none adjacent")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_500_1000_2500_2600_6000_6500_8000_9000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_2500_3000_8000_9000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_4500_5000_6500_6600_both), "Same chromosome, same orientation, interleaved exons some adjacent")
    assert(!chr1_1000_2000_3000_4000_5000_6000_plus.contains(chr1_7000_8000_9000_10000_plus), "Same chromosome, same orientation, non-overlapping spans")
    assert(!chr1_1000_2000_3000_4000_5000_6000_plus.contains(chr1_6000_8000_9000_10000_plus), "Same chromosome, same orientation, non-overlapping spans")
    assert(chr1_1000_2000_3000_4000_5000_6000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_plus), "Self")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_1500_1600_3500_3600_5500_6500_plus), "Three blocks nested in three blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_500_600_1500_1600_3500_4500_5500_6500_plus), "Three blocks nested in three blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus), "Three blocks nested in three blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus), "Three blocks nested in three blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus), "Three blocks nested in three blocks")
    assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_1100_1200_1300_1400_3500_3600_plus), "Three blocks nested in two blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus), "Three blocks nested in two blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus), "Three blocks nested in two blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_500_1500_2500_3500_4500_5500_plus), "Three blocks overlapping three blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus), "Three blocks overlapping three blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus), "Three blocks overlapping three blocks")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_3000_3100_3200_3300_4500_4600_plus), "Two blocks nested in one block")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_500_4500_5100_5200_5300_5400_plus), "Two blocks nested in one block")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_3000_3100_3200_3300_4500_5000_plus), "Two blocks nested in one block")
    assert(!chr1_1000_2000_3000_4000_5000_6000_both.contains(chr1_500_5000_5100_5200_5300_5400_plus), "Two blocks nested in one block")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_500_600_3000_4000_6500_6600_7000_8000_plus), "Two blocks same")
    assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.contains(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus), "Two blocks same")
    assert(!chr1_500_600_1500_1600_plus.contains(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
    assert(!chr1_2000_3000_3500_3600_both.contains(chr1_1000_2000_3000_4000_plus), "One block nested, others non-overlapping")
    assert(!chr1_500_1500_3500_3600_7000_8000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
    assert(!chr1_1000_2000_3500_3600_plus.contains(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
    assert(chr1_500_6500_7000_8000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
    assert(!chr1_1500_5500_7000_8000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
    assert(!chr1_2000_8500_9000_10000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
    assert(!chr1_500_4500_7000_8000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
    assert(!chr1_500_4000_4500_4600_6500_6600_plus.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
    assert(chr1_1000_2000_3000_4000_5000_6000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_both), "Same blocks compatible orientation")
    assert(!chr1_7000_8000_9000_10000_minus.contains(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, different orientation, non-overlapping spans")
    assert(!chr1_1000_4000_7000_8000_minus.contains(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, different orientation, overlapping spans")
    assert(!chr1_6000_7000_8000_9000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, adjacent spans")
    assert(!chr1_2000_3000_4000_5000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, interleaved exons completely filling span")
    assert(!chr1_2000_3000_4000_5000_6000_7000_both.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons completely filling span")
    assert(!chr1_2000_3000_8000_9000_both.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
    assert(!chr1_2000_3000_4000_5000_8000_9000_both.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
    assert(!chr1_900_1000_4000_5000_6000_7000_8000_9000_both.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
    assert(!chr1_2500_2600_4500_4600_6500_6600_both.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons none adjacent")
    assert(!chr1_4500_4600_6500_6600_8500_8600_both.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons none adjacent")
    assert(!chr1_500_1000_2500_2600_6000_6500_8000_9000_both.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
    assert(!chr1_2500_3000_8000_9000_both.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
    assert(!chr1_4500_5000_6500_6600_both.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
    assert(!chr1_7000_8000_9000_10000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, non-overlapping spans")
    assert(!chr1_6000_8000_9000_10000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, non-overlapping spans")
    assert(!chr1_1500_1600_3500_3600_5500_6500_plus.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
    assert(chr1_1000_2000_2500_6500_7000_8000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Two blocks same")
    assert(!chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.contains(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Two blocks same")
    assert(!chr1_900_1100_1900_2100_2900_6000_plus.contains(chr2_900_1100_1900_2100_2900_6000_plus), "Different chromosomes")
    assert(chr1_1000_2000_3000_4000_5000_6000_plus.contains(chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus), "Nested blocks")

  }

}
