package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class RegionOverlapSuite extends FunSuite {

  overlapBlockBlock()
  overlapBlockSetBlock()
  overlapBlockSetBlockSet()
  overlapEmpty()

  overlapCompatibleIntronsBlockBlock()
  overlapCompatibleIntronsBlockSetBlock()
  overlapCompatibleIntronsBlockSetBlockSet()
  overlapCompatibleIntronsEmpty()


  def overlapEmpty(): Unit = {
    test("Empty overlap") {
      assert(!Empty.overlaps(Empty), "Empty shouldn't overlap anything")
      assert(!Empty.overlaps(chr1_1000_2000_both), "Empty shouldn't overlap anything")
      assert(!Empty.overlaps(chr1_100_200_300_400_plus), "Empty shouldn't overlap anything")
      assert(!chr1_1000_2000_both.overlaps(Empty), "Empty shouldn't overlap anything")
      assert(!chr1_100_200_300_400_plus.overlaps(Empty), "Empty shouldn't overlap anything")
      assert(!Empty.overlapsSpan(Empty), "Empty shouldn't overlap anything")
      assert(!Empty.overlapsSpan(chr1_1000_2000_both), "Empty shouldn't overlap anything")
      assert(!Empty.overlapsSpan(chr1_100_200_300_400_plus), "Empty shouldn't overlap anything")
      assert(!chr1_1000_2000_both.overlapsSpan(Empty), "Empty shouldn't overlap anything")
      assert(!chr1_100_200_300_400_plus.overlapsSpan(Empty), "Empty shouldn't overlap anything")
    }
  }

  def overlapCompatibleIntronsEmpty(): Unit = {
    test("Empty overlap compatible introns") {
      assert(!Empty.overlapsCompatibleIntrons(Empty), "Empty shouldn't overlap anything")
      assert(!Empty.overlapsCompatibleIntrons(chr1_1000_2000_both), "Empty shouldn't overlap anything")
      assert(!Empty.overlapsCompatibleIntrons(chr1_100_200_300_400_plus), "Empty shouldn't overlap anything")
      assert(!chr1_1000_2000_both.overlapsCompatibleIntrons(Empty), "Empty shouldn't overlap anything")
      assert(!chr1_100_200_300_400_plus.overlapsCompatibleIntrons(Empty), "Empty shouldn't overlap anything")
    }
  }

  def overlapBlockBlock(): Unit = {
    test("Block overlap with block") {

      // Empty
      assert(!Empty.overlaps(chr1_1000_2000_plus_1), "Empty shouldn't overlap anything")
      assert(!chr1_1000_2000_plus_1.overlaps(Empty), "Empty shouldn't overlap anything")
      assert(!chr1_1000_2000_plus_1.overlapsSpan(Empty), "Empty shouldn't overlap anything")
      assert(!Empty.overlaps(Empty), "Empty shouldn't overlap itself")

      // Self overlap
      assert(chr1_1000_2000_both.overlaps(chr1_1000_2000_both), "Block should overlap itself")
      assert(chr1_1500_2500_plus.overlaps(chr1_1500_2500_plus), "Block should overlap itself")
      assert(chr1_1000_2000_minus.overlaps(chr1_1000_2000_minus), "Block should overlap itself")
      assert(chr1_1000_2000_both.overlaps(chr1_1000_2000_both), "Block should overlap itself")
      assert(chr1_1000_2000_both.overlapsSpan(chr1_1000_2000_both), "Block should overlap itself")

      // Overlapping blocks with compatible orientations
      assert(chr1_1000_2000_plus_1.overlaps(chr1_1500_2500_plus), "Overlapping plus blocks should overlap")
      assert(chr1_1500_2500_plus.overlaps(chr1_1000_2000_plus_1), "Overlapping plus blocks should overlap")
      assert(chr1_1000_2000_minus.overlaps(chr1_1500_2500_minus), "Overlapping minus blocks should overlap")
      assert(chr1_1500_2500_minus.overlaps(chr1_1000_2000_minus), "Overlapping minus blocks should overlap")
      assert(chr1_1500_2500_minus.overlapsSpan(chr1_1000_2000_minus), "Overlapping minus blocks should overlap")
      assert(chr1_1000_2000_both.overlaps(chr1_1500_2500_both), "Overlapping both blocks should overlap")
      assert(chr1_1500_2500_both.overlaps(chr1_1000_2000_both), "Overlapping both blocks should overlap")
      assert(chr1_1000_2000_both.overlaps(chr1_1500_2500_both), "Overlapping unstranded blocks should overlap")
      assert(chr1_1500_2500_both.overlaps(chr1_1000_2000_both), "Overlapping unstranded blocks should overlap")
      assert(chr1_1000_2000_plus_1.overlaps(chr1_1500_2500_both), "Plus and both block should overlap")
      assert(chr1_1500_2500_both.overlaps(chr1_1000_2000_plus_1), "Plus and both block should overlap")
      assert(chr1_1000_2000_minus.overlaps(chr1_1500_2500_both), "Minus and both block should overlap")
      assert(chr1_1500_2500_both.overlaps(chr1_1000_2000_minus), "Minus and both block should overlap")

      // Overlapping blocks with non-compatible orientations
      assert(!chr1_1000_2000_plus_1.overlaps(chr1_1000_2000_minus), "Plus and minus blocks should not overlap")
      assert(!chr1_1000_2000_minus.overlaps(chr1_1000_2000_plus_1), "Plus and minus blocks should not overlap")
      assert(!chr1_1000_2000_minus.overlapsSpan(chr1_1000_2000_plus_1), "Plus and minus blocks should not overlap")

      // Non-overlapping blocks
      assert(!chr1_1000_2000_plus_1.overlaps(chr1_2000_3000_plus), "Adjacent blocks should not overlap")
      assert(!chr1_1000_2000_plus_1.overlapsSpan(chr1_2000_3000_plus), "Adjacent blocks should not overlap")
      assert(!chr1_1000_2000_plus_1.overlaps(chr2_1000_2000_plus), "Blocks with same coordinates on different chromosomes should not overlap")

      // Single position overlap
      assert(chr1_4_6_minus.overlaps(chr1_5_6_minus), "Single position overlap should overlap")
      assert(chr1_4_6_minus.overlapsSpan(chr1_5_6_minus), "Single position overlap should overlap")
      assert(!chr1_5_6_minus.overlaps(chr1_6_7_minus), "Single position adjacent should not overlap")
      assert(!chr1_5_6_minus.overlapsSpan(chr1_6_7_minus), "Single position adjacent should not overlap")

    }
  }

  def overlapCompatibleIntronsBlockBlock(): Unit = {
    test("Overlap compatible introns block block") {

      // Self overlap
      assert(chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1000_2000_both), "Block should overlap itself")
      assert(chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1000_2000_both), "Block should overlap itself")

      // Overlapping blocks with compatible orientations
      assert(chr1_1000_2000_plus_1.overlapsCompatibleIntrons(chr1_1500_2500_plus), "Overlapping plus blocks should overlap")
      assert(chr1_1000_2000_plus_1.overlapsCompatibleIntrons(chr1_1500_2500_both), "Plus and both block should overlap")
      assert(chr1_1500_2500_both.overlapsCompatibleIntrons(chr1_1000_2000_minus), "Minus and both block should overlap")

      // Overlapping blocks with non-compatible orientations
      assert(!chr1_1000_2000_plus_1.overlapsCompatibleIntrons(chr1_1000_2000_minus), "Plus and minus blocks should not overlap")

      // Non-overlapping blocks
      assert(!chr1_1000_2000_plus_1.overlapsCompatibleIntrons(chr1_2000_3000_plus), "Adjacent blocks should not overlap")
      assert(!chr1_1000_2000_plus_1.overlapsCompatibleIntrons(chr2_1000_2000_plus), "Blocks with same coordinates on different chromosomes should not overlap")

      // Single position overlap
      assert(chr1_4_6_minus.overlapsCompatibleIntrons(chr1_5_6_minus), "Single position overlap should overlap")
      assert(!chr1_5_6_minus.overlapsCompatibleIntrons(chr1_6_7_minus), "Single position adjacent should not overlap")

    }
  }

  def overlapBlockSetBlock(): Unit = {
    test("BlockSet overlap with block") {

      /*
      Call on the block set
       */

      // Strictly nested both directions
      assert(chr1_900_2100_3000_4000_plus.overlaps(chr1_1000_2000_both), "One block of block set completely contains the other block")
      assert(chr1_1100_1200_1300_1400_plus.overlaps(chr1_1000_2000_both), "Block completely contains block set")
      assert(chr1_1100_1200_1300_1400_plus.overlapsSpan(chr1_1000_2000_both), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_1100_1200_1300_plus.overlaps(chr1_1000_2000_both), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_1100_1900_2000_plus.overlaps(chr1_1000_2000_both), "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlaps(chr1_1000_2000_both), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlaps(Block("1", 1000, 1050, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlaps(Block("1", 1050, 1100, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlaps(Block("1", 1200, 1250, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlapsSpan(Block("1", 1200, 1250, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlaps(Block("1", 1250, 1300, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlaps(Block("1", 1900, 1950, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlaps(Block("1", 1950, 2000, Plus)), "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1900_2100_3000_4000_plus.overlaps(chr1_1000_2000_both), "Overlapping one block off end")
      assert(chr1_1900_2100_3000_4000_plus.overlaps(chr1_1500_2500_plus), "Overlapping one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1400, 1550, Plus)), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1550, 1650, Plus)), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1650, 1750, Plus)), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1750, 1850, Plus)), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 2000, 2150, Plus)), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 2200, 2350, Plus)), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsSpan(Block("1", 2200, 2350, Plus)), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1700_1800_1900_2100_plus.overlaps(chr1_1000_2000_both), "Overlapping two blocks off end")
      assert(chr1_1700_1800_1900_2100_plus.overlaps(Block("1", 1750, 2200, Plus)), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1400, 1750, Plus)), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1550, 1850, Plus)), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1650, 2200, Plus)), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1750, 2400, Plus)), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1500_1600_2100_2300_plus.overlapsSpan(chr1_1000_2000_both), "Overlapping one block and an intron")
      assert(chr1_1500_1600_2100_2300_plus.overlaps(chr1_1000_2000_both), "Overlapping one block and an intron")
      assert(chr1_1500_1600_2100_2300_plus.overlaps(chr1_2000_3000_plus), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(chr1_1000_2000_both), "Overlapping two blocks and an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1650, 2400, Plus)), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlaps(Block("1", 500, 6500, Plus)), "Overlapping three blocks and an intron")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlaps(Block("1", 2500, 8500, Plus)), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_1000_2000_both), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1500, 1600, Plus)), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1700, 1800, Plus)), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 2100, 2300, Plus)), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_900_1100_1500_1600_1900_2100_plus.overlaps(chr1_1000_2000_both), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlaps(Block("1", 1500, 7500, Plus)), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1400, 1600, Plus)), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1500, 1650, Plus)), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1650, 1800, Plus)), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1700, 1850, Plus)), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 2000, 2300, Plus)), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 2100, 2400, Plus)), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(!chr1_100_200_300_400_plus.overlaps(chr1_1000_2000_plus_1), "Non-overlapping because different spans")
      assert(!chr1_100_200_300_400_plus.overlapsSpan(chr1_1000_2000_plus_1), "Non-overlapping because different spans")
      assert(!chr1_100_200_300_400_plus.overlaps(Block("1", 500, 600, Unstranded)), "Non-overlapping because different spans")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1000, 1100, Unstranded)), "Non-overlapping because different spans")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 2400, 2500, Unstranded)), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(chr1_1000_2000_minus), "Non-overlapping because different orientations")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsSpan(chr1_1000_2000_minus), "Non-overlapping because different orientations")
      // Different chromosomes
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.overlaps(chr2_1000_2000_plus), "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(!chr1_100_200_300_400_plus.overlaps(Block("1", 200, 300, Plus)), "Same span as an intron")
      assert(chr1_100_200_300_400_plus.overlapsSpan(Block("1", 200, 300, Plus)), "Same span as an intron")
      // Three blocks
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1600, 1700, Plus)), "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsSpan(Block("1", 1600, 1700, Plus)), "Same span as an intron")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1800, 2100, Plus)), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(!chr1_100_200_300_400_plus.overlaps(Block("1", 50, 100, Plus)), "Adjacent to span")
      assert(!chr1_100_200_300_400_plus.overlaps(Block("1", 400, 500, Plus)), "Adjacent to span")
      assert(!chr1_100_200_300_400_plus.overlapsSpan(Block("1", 400, 500, Plus)), "Adjacent to span")
      // Three blocks
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1400, 1500, Plus)), "Same span as an intron")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 2300, 2400, Plus)), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(!chr1_100_200_300_400_plus.overlaps(Block("1", 201, 300, Plus)), "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.overlapsSpan(Block("1", 201, 300, Plus)), "1bp gap between blocks")
      assert(!chr1_100_200_300_400_plus.overlaps(Block("1", 200, 299, Plus)), "1bp gap between blocks")
      assert(!chr1_100_200_300_400_plus.overlaps(Block("1", 201, 299, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1801, 2099, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1601, 1699, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1801, 2099, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1600, 1699, Plus)), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsSpan(Block("1", 1600, 1699, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1800, 2099, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1601, 1700, Plus)), "1bp gap between blocks")
      // Outside span
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1400, 1499, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 2301, 2400, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 1400, 1500, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlaps(Block("1", 2300, 2400, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsSpan(Block("1", 2300, 2400, Plus)), "1bp gap between blocks")


      /*
       Call on the block
        */

      // Strictly nested both directions
      assert(chr1_1000_2000_both.overlaps(chr1_900_2100_3000_4000_plus), "One block of block set completely contains the other block")
      assert(chr1_1000_2000_both.overlaps(chr1_1100_1200_1300_1400_plus), "Block completely contains block set")
      assert(chr1_1000_2000_both.overlapsSpan(chr1_1100_1200_1300_1400_plus), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_2000_both.overlaps(chr1_1000_1100_1200_1300_plus), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_2000_both.overlaps(chr1_1000_1100_1900_2000_plus), "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_2000_both.overlaps(chr1_1000_1100_1200_1300_1900_2000_plus), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(Block("1", 1000, 1050, Plus).overlaps(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1050, 1100, Plus).overlaps(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1200, 1250, Plus).overlaps(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1250, 1300, Plus).overlaps(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1250, 1300, Plus).overlapsSpan(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1900, 1950, Plus).overlaps(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1950, 2000, Plus).overlaps(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1000_2000_both.overlaps(chr1_1900_2100_3000_4000_plus), "Overlapping one block off end")
      assert(chr1_1500_2500_plus.overlaps(chr1_1900_2100_3000_4000_plus), "Overlapping one block")
      assert(Block("1", 1400, 1550, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(Block("1", 1550, 1650, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(Block("1", 1650, 1750, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(Block("1", 1750, 1850, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(Block("1", 2000, 2150, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(Block("1", 2200, 2350, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1000_2000_both.overlaps(chr1_1700_1800_1900_2100_plus), "Overlapping two blocks off end")
      assert(Block("1", 1750, 2200, Plus).overlaps(chr1_1700_1800_1900_2100_plus), "Overlapping two blocks off end")
      assert(Block("1", 1400, 1750, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks off end")
      assert(Block("1", 1550, 1850, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks off end")
      assert(Block("1", 1550, 1850, Plus).overlapsSpan(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks off end")
      assert(Block("1", 1650, 2200, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks off end")
      assert(Block("1", 1750, 2400, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1000_2000_both.overlaps(chr1_1500_1600_2100_2300_plus), "Overlapping one block and an intron")
      assert(chr1_2000_3000_plus.overlaps(chr1_1500_1600_2100_2300_plus), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1000_2000_both.overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks and an intron")
      assert(Block("1", 1650, 2400, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(Block("1", 500, 6500, Plus).overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus), "Overlapping three blocks and an intron")
      assert(Block("1", 2500, 8500, Plus).overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_both.overlaps(chr1_1000_2000_3000_4000_plus), "Same span as one block")
      assert(Block("1", 1500, 1600, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as one block")
      assert(Block("1", 1700, 1800, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as one block")
      assert(Block("1", 2100, 2300, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_1000_2000_both.overlaps(chr1_900_1100_1500_1600_1900_2100_plus), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(Block("1", 1500, 7500, Plus).overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(Block("1", 1400, 1600, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(Block("1", 1500, 1650, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(Block("1", 1650, 1800, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(Block("1", 1700, 1850, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(Block("1", 2000, 2300, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(Block("1", 2100, 2400, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(!chr1_1000_2000_plus_1.overlaps(chr1_100_200_300_400_plus), "Non-overlapping because different spans")
      assert(!Block("1", 500, 600, Unstranded).overlaps(chr1_100_200_300_400_plus), "Non-overlapping because different spans")
      assert(!Block("1", 1000, 1100, Unstranded).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Non-overlapping because different spans")
      assert(!Block("1", 2400, 2500, Unstranded).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Non-overlapping because different spans")
      assert(!Block("1", 2400, 2500, Unstranded).overlapsSpan(chr1_1500_1600_1700_1800_2100_2300_plus), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(!chr1_1000_2000_minus.overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Non-overlapping because different orientations")
      // Different chromosomes
      assert(!chr2_1000_2000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus), "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(!Block("1", 200, 300, Plus).overlaps(chr1_100_200_300_400_plus), "Same span as an intron")
      assert(Block("1", 200, 300, Plus).overlapsSpan(chr1_100_200_300_400_plus), "Same span as an intron")
      // Three blocks
      assert(!Block("1", 1600, 1700, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as an intron")
      assert(Block("1", 1600, 1700, Plus).overlapsSpan(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as an intron")
      assert(!Block("1", 1800, 2100, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(!Block("1", 50, 100, Plus).overlaps(chr1_100_200_300_400_plus), "Adjacent to span")
      assert(!Block("1", 50, 100, Plus).overlapsSpan(chr1_100_200_300_400_plus), "Adjacent to span")
      assert(!Block("1", 400, 500, Plus).overlaps(chr1_100_200_300_400_plus), "Adjacent to span")
      // Three blocks
      assert(!Block("1", 1400, 1500, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as an intron")
      assert(!Block("1", 2300, 2400, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(!Block("1", 201, 300, Plus).overlaps(chr1_100_200_300_400_plus), "1bp gap between blocks")
      assert(!Block("1", 200, 299, Plus).overlaps(chr1_100_200_300_400_plus), "1bp gap between blocks")
      assert(!Block("1", 201, 299, Plus).overlaps(chr1_100_200_300_400_plus), "1bp gap between blocks")
      assert(!Block("1", 1801, 2099, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1601, 1699, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1801, 2099, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(Block("1", 1801, 2099, Plus).overlapsSpan(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1600, 1699, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1800, 2099, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1601, 1700, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      // Outside span
      assert(!Block("1", 1400, 1499, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 2301, 2400, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1400, 1500, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 2300, 2400, Plus).overlaps(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 2300, 2400, Plus).overlapsSpan(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")

    }
  }

  def overlapCompatibleIntronsBlockSetBlock(): Unit = {
    test("Overlap compatible introns BlockSet with Block") {

      /*
      Call on the block set
       */

      // Strictly nested both directions
      assert(chr1_900_2100_3000_4000_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "One block of block set completely contains the other block")
      assert(!chr1_1100_1200_1300_1400_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(!chr1_1000_1100_1200_1300_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(!chr1_1000_1100_1900_2000_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Block completely contains block set, sharing two endpoints")
      assert(!chr1_1000_1100_1200_1300_1900_2000_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlapsCompatibleIntrons(Block("1", 1000, 1050, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlapsCompatibleIntrons(Block("1", 1050, 1100, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlapsCompatibleIntrons(Block("1", 1200, 1250, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlapsCompatibleIntrons(Block("1", 1250, 1300, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlapsCompatibleIntrons(Block("1", 1900, 1950, Plus)), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.overlapsCompatibleIntrons(Block("1", 1950, 2000, Plus)), "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1900_2100_3000_4000_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Overlapping one block off end")
      assert(!chr1_1900_2100_3000_4000_plus.overlapsCompatibleIntrons(chr1_1500_2500_plus), "Overlapping one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1400, 1550, Plus)), "Overlapping one block off end")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1550, 1650, Plus)), "Overlapping one block off end")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1650, 1750, Plus)), "Overlapping one block off end")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1750, 1850, Plus)), "Overlapping one block off end")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 2000, 2150, Plus)), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 2200, 2350, Plus)), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(!chr1_1700_1800_1900_2100_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Overlapping two blocks off end")
      assert(!chr1_1700_1800_1900_2100_plus.overlapsCompatibleIntrons(Block("1", 1750, 2200, Plus)), "Overlapping two blocks off end")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1400, 1750, Plus)), "Overlapping two blocks off end")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1550, 1850, Plus)), "Overlapping two blocks off end")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1650, 2200, Plus)), "Overlapping two blocks off end")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1750, 2400, Plus)), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(!chr1_1500_1600_2100_2300_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Overlapping one block and an intron")
      assert(!chr1_1500_1600_2100_2300_plus.overlapsCompatibleIntrons(chr1_2000_3000_plus), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Overlapping two blocks and an intron")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1650, 2400, Plus)), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlapsCompatibleIntrons(Block("1", 500, 6500, Plus)), "Overlapping three blocks and an intron")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlapsCompatibleIntrons(Block("1", 2500, 8500, Plus)), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_3000_4000_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1500, 1600, Plus)), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1700, 1800, Plus)), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 2100, 2300, Plus)), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(!chr1_900_1100_1500_1600_1900_2100_plus.overlapsCompatibleIntrons(chr1_1000_2000_both), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlapsCompatibleIntrons(Block("1", 1500, 7500, Plus)), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1400, 1600, Plus)), "Contains one block sharing an endpoint")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1500, 1650, Plus)), "Contains one block sharing an endpoint")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1650, 1800, Plus)), "Contains one block sharing an endpoint")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1700, 1850, Plus)), "Contains one block sharing an endpoint")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 2000, 2300, Plus)), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 2100, 2400, Plus)), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(!chr1_100_200_300_400_plus.overlapsCompatibleIntrons(chr1_1000_2000_plus_1), "Non-overlapping because different spans")
      assert(!chr1_100_200_300_400_plus.overlapsCompatibleIntrons(Block("1", 500, 600, Unstranded)), "Non-overlapping because different spans")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1000, 1100, Unstranded)), "Non-overlapping because different spans")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 2400, 2500, Unstranded)), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(chr1_1000_2000_minus), "Non-overlapping because different orientations")
      // Different chromosomes
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.overlapsCompatibleIntrons(chr2_1000_2000_plus), "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(!chr1_100_200_300_400_plus.overlapsCompatibleIntrons(Block("1", 200, 300, Plus)), "Same span as an intron")
      // Three blocks
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1600, 1700, Plus)), "Same span as an intron")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1800, 2100, Plus)), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(!chr1_100_200_300_400_plus.overlapsCompatibleIntrons(Block("1", 50, 100, Plus)), "Adjacent to span")
      assert(!chr1_100_200_300_400_plus.overlapsCompatibleIntrons(Block("1", 400, 500, Plus)), "Adjacent to span")
      // Three blocks
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1400, 1500, Plus)), "Same span as an intron")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 2300, 2400, Plus)), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(!chr1_100_200_300_400_plus.overlapsCompatibleIntrons(Block("1", 201, 300, Plus)), "1bp gap between blocks")
      assert(!chr1_100_200_300_400_plus.overlapsCompatibleIntrons(Block("1", 200, 299, Plus)), "1bp gap between blocks")
      assert(!chr1_100_200_300_400_plus.overlapsCompatibleIntrons(Block("1", 201, 299, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1801, 2099, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1601, 1699, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1801, 2099, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1600, 1699, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1800, 2099, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1601, 1700, Plus)), "1bp gap between blocks")
      // Outside span
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1400, 1499, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 2301, 2400, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 1400, 1500, Plus)), "1bp gap between blocks")
      assert(!chr1_1500_1600_1700_1800_2100_2300_plus.overlapsCompatibleIntrons(Block("1", 2300, 2400, Plus)), "1bp gap between blocks")


      /*
       Call on the block
        */

      // Strictly nested both directions
      assert(chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_900_2100_3000_4000_plus), "One block of block set completely contains the other block")
      assert(!chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1100_1200_1300_1400_plus), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(!chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1000_1100_1200_1300_plus), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(!chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1000_1100_1900_2000_plus), "Block completely contains block set, sharing two endpoints")
      assert(!chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1000_1100_1200_1300_1900_2000_plus), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(Block("1", 1000, 1050, Plus).overlapsCompatibleIntrons(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1050, 1100, Plus).overlapsCompatibleIntrons(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1200, 1250, Plus).overlapsCompatibleIntrons(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1250, 1300, Plus).overlapsCompatibleIntrons(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1900, 1950, Plus).overlapsCompatibleIntrons(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1950, 2000, Plus).overlapsCompatibleIntrons(chr1_1000_1100_1200_1300_1900_2000_plus), "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1900_2100_3000_4000_plus), "Overlapping one block off end")
      assert(!chr1_1500_2500_plus.overlapsCompatibleIntrons(chr1_1900_2100_3000_4000_plus), "Overlapping one block")
      assert(Block("1", 1400, 1550, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(!Block("1", 1550, 1650, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(!Block("1", 1650, 1750, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(!Block("1", 1750, 1850, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(!Block("1", 2000, 2150, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      assert(Block("1", 2200, 2350, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(!chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1700_1800_1900_2100_plus), "Overlapping two blocks off end")
      assert(!Block("1", 1750, 2200, Plus).overlapsCompatibleIntrons(chr1_1700_1800_1900_2100_plus), "Overlapping two blocks off end")
      assert(!Block("1", 1400, 1750, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks off end")
      assert(!Block("1", 1550, 1850, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks off end")
      assert(!Block("1", 1650, 2200, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks off end")
      assert(!Block("1", 1750, 2400, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(!chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1500_1600_2100_2300_plus), "Overlapping one block and an intron")
      assert(!chr1_2000_3000_plus.overlapsCompatibleIntrons(chr1_1500_1600_2100_2300_plus), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(!chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks and an intron")
      assert(!Block("1", 1650, 2400, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(!Block("1", 500, 6500, Plus).overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus), "Overlapping three blocks and an intron")
      assert(!Block("1", 2500, 8500, Plus).overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_plus), "Same span as one block")
      assert(Block("1", 1500, 1600, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as one block")
      assert(Block("1", 1700, 1800, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as one block")
      assert(Block("1", 2100, 2300, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(!chr1_1000_2000_both.overlapsCompatibleIntrons(chr1_900_1100_1500_1600_1900_2100_plus), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(!Block("1", 1500, 7500, Plus).overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(Block("1", 1400, 1600, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(!Block("1", 1500, 1650, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(!Block("1", 1650, 1800, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(!Block("1", 1700, 1850, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(!Block("1", 2000, 2300, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      assert(Block("1", 2100, 2400, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(!chr1_1000_2000_plus_1.overlapsCompatibleIntrons(chr1_100_200_300_400_plus), "Non-overlapping because different spans")
      assert(!Block("1", 500, 600, Unstranded).overlapsCompatibleIntrons(chr1_100_200_300_400_plus), "Non-overlapping because different spans")
      assert(!Block("1", 1000, 1100, Unstranded).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Non-overlapping because different spans")
      assert(!Block("1", 2400, 2500, Unstranded).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(!chr1_1000_2000_minus.overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Non-overlapping because different orientations")
      // Different chromosomes
      assert(!chr2_1000_2000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus), "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(!Block("1", 200, 300, Plus).overlapsCompatibleIntrons(chr1_100_200_300_400_plus), "Same span as an intron")
      // Three blocks
      assert(!Block("1", 1600, 1700, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as an intron")
      assert(!Block("1", 1800, 2100, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(!Block("1", 50, 100, Plus).overlapsCompatibleIntrons(chr1_100_200_300_400_plus), "Adjacent to span")
      assert(!Block("1", 400, 500, Plus).overlapsCompatibleIntrons(chr1_100_200_300_400_plus), "Adjacent to span")
      // Three blocks
      assert(!Block("1", 1400, 1500, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as an intron")
      assert(!Block("1", 2300, 2400, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(!Block("1", 201, 300, Plus).overlapsCompatibleIntrons(chr1_100_200_300_400_plus), "1bp gap between blocks")
      assert(!Block("1", 200, 299, Plus).overlapsCompatibleIntrons(chr1_100_200_300_400_plus), "1bp gap between blocks")
      assert(!Block("1", 201, 299, Plus).overlapsCompatibleIntrons(chr1_100_200_300_400_plus), "1bp gap between blocks")
      assert(!Block("1", 1801, 2099, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1601, 1699, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1801, 2099, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1600, 1699, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1800, 2099, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1601, 1700, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      // Outside span
      assert(!Block("1", 1400, 1499, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 2301, 2400, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 1400, 1500, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")
      assert(!Block("1", 2300, 2400, Plus).overlapsCompatibleIntrons(chr1_1500_1600_1700_1800_2100_2300_plus), "1bp gap between blocks")

    }
  }

 def overlapCompatibleIntronsBlockSetBlockSet(): Unit = {
    test("Overlap compatible introns BlockSet with BlockSet") {
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlapsCompatibleIntrons(BlockSet(List(
        Block("1", 500, 2000, Plus),
        Block("1", 3000, 4000, Plus),
        Block("1", 5000, 6000, Plus),
        Block("1", 7000, 8000, Plus)))))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlapsCompatibleIntrons(BlockSet(List(
        Block("1", 1500, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)))))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlapsCompatibleIntrons(BlockSet(List(
        Block("1", 3500, 4000, Plus),
        Block("1", 5000, 6000, Plus),
        Block("1", 7000, 7500, Plus)))))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.overlapsCompatibleIntrons(BlockSet(List(
        Block("1", 3500, 4000, Unstranded),
        Block("1", 5000, 5001, Unstranded)))))
      assert(BlockSet(List(
        Block("1", 3000, 4000, Plus),
        Block("1", 5000, 6000, Plus),
        Block("1", 7000, 8500, Plus),
        Block("1", 9000, 10000, Plus))).overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus))
      assert(BlockSet(List(
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded))).overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus))
      assert(BlockSet(List(
        Block("1", 5500, 6000, Plus),
        Block("1", 7000, 8500, Plus),
        Block("1", 9000, 10000, Plus),
        Block("1", 11000, 12000, Plus))).overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus))
      assert(BlockSet(List(
        Block("1", 500, 600, Unstranded),
        Block("1", 700, 800, Unstranded),
        Block("1", 900, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded))).overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus))
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_600_1500_1600_plus), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_600_2500_2600_5500_5600_plus), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2500_2600_3500_3600_4500_4600_plus), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2500_2600_3500_3600_6500_6600_plus), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_600_700_1000_1500_1600_plus), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2000_3000_3500_3600_4500_4600_plus), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2000_2600_3500_3600_6500_6600_plus), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_4000_5000_5500_5600_6000_6600_plus), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_5500_5600_6000_6600_7000_8000_plus), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_plus.overlapsCompatibleIntrons(chr1_500_600_3500_3600_both), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_plus.overlapsCompatibleIntrons(chr1_1100_1200_2500_3000_both), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_plus.overlapsCompatibleIntrons(chr1_1100_1200_4000_4600_both), "One block nested, others non-overlapping")
      assert(!chr1_1000_2000_3000_4000_plus.overlapsCompatibleIntrons(chr1_2000_3000_3500_3600_both), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_600_700_800_900_1100_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_600_700_800_1000_2000_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_600_700_800_900_2000_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.overlapsCompatibleIntrons(chr1_500_600_900_1100_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.overlapsCompatibleIntrons(chr1_500_600_1000_2000_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.overlapsCompatibleIntrons(chr1_500_600_900_2000_both), "One block overlapping off end of span")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_1500_2500_4000_5000_plus), "One block overlapping one block of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2500_3500_7000_8000_plus), "One block overlapping one block of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_4500_4600_5500_6500_7000_8000_plus), "One block overlapping one block of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2000_3000_4000_5000_5500_5600_plus), "One block overlapping one block of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2100_2200_2300_2400_2500_3100_plus), "One block overlapping one block of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_1500_3500_3600_7000_8000_plus), "One block overlapping one block, another block nested in another block")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_1500_2500_2600_3500_3600_plus), "One block overlapping one block, another block nested in another block")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2500_2600_3500_3600_3800_4500_plus), "One block overlapping one block, another block nested in another block")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_3000_3500_3600_7000_8000_plus), "One block overlapping one block, another block nested in another block")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_1500_2500_2600_3500_3600_plus), "One block overlapping one block, another block nested in another block")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_3500_3600_4000_6000_plus), "One block overlapping one block, another block nested in another block")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_6500_7000_8000_plus), "One block overlapping three blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_600_1500_5500_plus), "One block overlapping three blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_2500_2600_2800_9000_plus), "One block overlapping three blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_1000_1500_7000_plus), "One block overlapping three blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_500_600_2000_9000_plus), "One block overlapping three blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_4500_7000_8000_plus), "One block overlapping two blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_3000_6000_7000_8000_plus), "One block overlapping two blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_3500_4500_5500_6500_9000_10000_plus), "One block overlapping two blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_5000_7000_8000_plus), "One block overlapping two blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2000_6000_7000_8000_plus), "One block overlapping two blocks of other")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_3500_5000_5500_6500_9000_10000_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_600_1000_2000_plus), "One block same")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_3000_4000_5500_6500_plus), "One block same")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2500_2600_4500_4600_5000_6000_plus), "One block same")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_1500_2500_3500_4500_5000_6000_plus), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_5000_6000_7000_8000_plus), "One block same")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_2500_3000_4500_4600_5000_6000_plus), "One block same")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_1500_2500_3000_4500_5000_6000_plus), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_plus), "Same blocks compatible orientation")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlapsCompatibleIntrons(chr1_7000_8000_9000_10000_minus), "Same chromosome, different orientation, non-overlapping spans")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlapsCompatibleIntrons(chr1_1000_4000_7000_8000_minus), "Same chromosome, different orientation, overlapping spans")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlapsCompatibleIntrons(chr1_6000_7000_8000_9000_plus), "Same chromosome, same orientation, adjacent spans")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlapsCompatibleIntrons(chr1_2000_3000_4000_5000_plus), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_2000_3000_4000_5000_6000_7000_both), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_2000_3000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_2000_3000_4000_5000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_900_1000_4000_5000_6000_7000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_2500_2600_4500_4600_6500_6600_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_4500_4600_6500_6600_8500_8600_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_500_1000_2500_2600_6000_6500_8000_9000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_2500_3000_8000_9000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_4500_5000_6500_6600_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlapsCompatibleIntrons(chr1_7000_8000_9000_10000_plus), "Same chromosome, same orientation, non-overlapping spans")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlapsCompatibleIntrons(chr1_6000_8000_9000_10000_plus), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_plus), "Self")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_1500_1600_3500_3600_5500_6500_plus), "Three blocks nested in three blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus), "Three blocks nested in three blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus), "Three blocks nested in three blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus), "Three blocks nested in two blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus), "Three blocks nested in two blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus), "Three blocks overlapping three blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus), "Three blocks overlapping three blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_3500_4500_5000_5100_5900_6000_plus), "Three blocks overlapping two blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_1500_3500_3600_3700_3800_5000_plus), "Three blocks overlapping two blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_3000_3100_3200_3300_4500_4600_plus), "Two blocks nested in one block")
      assert(!chr1_1000_2000_3000_4000_5000_6000_both.overlapsCompatibleIntrons(chr1_500_5000_5100_5200_5300_5400_plus), "Two blocks nested in one block")
      assert(!chr1_1000_2000_3000_4000_plus.overlapsCompatibleIntrons(chr1_1500_2500_3500_4500_plus), "Two blocks overlapping two blocks")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_500_600_3000_4000_6500_6600_7000_8000_plus), "Two blocks same")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_1000_2000_2500_6500_7000_8000_plus), "Two blocks same")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsCompatibleIntrons(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus), "Two blocks same")
      assert(!chr1_500_600_1500_1600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(!chr1_500_600_700_800_5500_5600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(!chr1_2500_2600_3500_3600_4500_4600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(!chr1_5500_5600_6500_6600_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(!chr1_500_600_700_1000_3500_3600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(!chr1_500_600_2500_3000_5500_5600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(!chr1_2500_3000_5500_5600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(!chr1_5500_5600_6000_6600_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(!chr1_1100_1200_4000_4600_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_plus), "One block nested, others non-overlapping")
      assert(!chr1_2000_3000_3500_3600_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_plus), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_900_1100_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_1000_2000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_900_2000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping off end of span")
      assert(chr1_500_600_900_1100_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_plus), "One block overlapping off end of span")
      assert(chr1_500_600_1000_2000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_plus), "One block overlapping off end of span")
      assert(chr1_500_600_900_2000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_plus), "One block overlapping off end of span")
      assert(!chr1_1500_2500_4000_5000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(!chr1_2500_3500_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(!chr1_3000_4000_4500_5500_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(!chr1_500_600_4000_6000_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_5000_6000_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(!chr1_500_1000_5000_7000_8000_9000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(!chr1_2100_2200_2300_2400_2500_3100_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(!chr1_500_1500_3500_3600_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(!chr1_500_1500_2500_2600_3500_3600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(!chr1_3500_3600_4500_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(!chr1_3000_4500_5500_5600_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(!chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(!chr1_1500_5500_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
      assert(!chr1_500_5500_6500_6600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(!chr1_2500_8500_9000_10000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(!chr1_500_1000_1500_5500_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
      assert(!chr1_2000_2600_2800_9000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(!chr1_2500_3500_5500_5600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(!chr1_500_3500_4500_4600_6500_6600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(!chr1_4500_5500_6500_7500_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(!chr1_2500_5000_5500_5600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(!chr1_500_4000_4500_4600_6500_6600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(!chr1_4500_6000_6500_7500_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_500_600_1000_2000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(!chr1_3000_4000_5500_6500_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(!chr1_2500_2600_4500_4600_5000_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(!chr1_1500_2500_3500_4500_5000_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(chr1_5000_6000_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(!chr1_2500_3000_4500_4600_5000_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(!chr1_1500_2500_3000_4500_5000_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "Same blocks compatible orientation")
      assert(!chr1_7000_8000_9000_10000_minus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, different orientation, non-overlapping spans")
      assert(!chr1_1000_4000_7000_8000_minus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, different orientation, overlapping spans")
      assert(!chr1_6000_7000_8000_9000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, adjacent spans")
      assert(!chr1_2000_3000_4000_5000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(!chr1_2000_3000_4000_5000_6000_7000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(!chr1_2000_3000_8000_9000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_2000_3000_4000_5000_8000_9000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_900_1000_4000_5000_6000_7000_8000_9000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_2500_2600_4500_4600_6500_6600_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(!chr1_4500_4600_6500_6600_8500_8600_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(!chr1_500_1000_2500_2600_6000_6500_8000_9000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_2500_3000_8000_9000_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_4500_5000_6500_6600_both.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_7000_8000_9000_10000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, non-overlapping spans")
      assert(!chr1_6000_8000_9000_10000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_plus), "Self")
      assert(!chr1_1500_1600_3500_3600_5500_6500_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(!chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(!chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(!chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in two blocks")
      assert(!chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in two blocks")
      assert(!chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping three blocks")
      assert(!chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping three blocks")
      assert(!chr1_3500_4500_5000_5100_5900_6000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(!chr1_1500_3500_3600_3700_3800_5000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(!chr1_3000_3100_3200_3300_4500_4600_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "Two blocks nested in one block")
      assert(!chr1_500_5000_5100_5200_5300_5400_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_both), "Two blocks nested in one block")
      assert(!chr1_1500_2500_3500_4500_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_plus), "Two blocks overlapping two blocks")
      assert(!chr1_500_600_3000_4000_6500_6600_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Two blocks same")
      assert(!chr1_1000_2000_2500_6500_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Two blocks same")
      assert(!chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.overlapsCompatibleIntrons(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Two blocks same")
      assert(!chr1_900_1100_1900_2100_2900_6000_plus.overlapsCompatibleIntrons(chr2_900_1100_1900_2100_2900_6000_plus), "Different chromosomes")
    }
  }

  def overlapBlockSetBlockSet(): Unit = {
    test("BlockSet overlap with BlockSet") {
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_1500_1600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_700_800_1500_1600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_700_800_3500_3600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_700_800_5500_5600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_2500_2600_3500_3600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_2500_2600_5500_5600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_2600_3500_3600_4500_4600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlapsSpan(chr1_2500_2600_3500_3600_4500_4600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_2600_3500_3600_6500_6600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_2600_5500_5600_6500_6600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_5500_5600_6500_6600_7000_8000_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_1000_1500_1600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_700_1000_1500_1600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_700_1000_3500_3600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_700_1000_5500_5600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_2000_3000_3500_3600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_2500_3000_5500_5600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2000_3000_3500_3600_4500_4600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2000_2600_3500_3600_6500_6600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_3000_5500_5600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_4000_4600_5500_5600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_4000_5000_5500_5600_6000_6600_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_5500_5600_6000_6600_7000_8000_plus), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_500_600_3500_3600_both), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_1100_1200_2500_3000_both), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_1100_1200_4000_4600_both), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_2000_3000_3500_3600_both), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_700_800_900_1100_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_700_800_1000_2000_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_700_800_900_2000_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_500_600_900_1100_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_500_600_1000_2000_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_500_600_900_2000_both), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_1500_2500_4000_5000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_3500_7000_8000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3000_4000_4500_5500_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_2600_2700_3500_6000_7000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_3500_4000_7000_8000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_4000_6000_7000_8000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_2000_5000_7000_8000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3500_5000_6000_7000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_5000_6000_7000_8000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_1000_5000_7000_8000_9000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_4500_4600_5500_6500_7000_8000_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2000_3000_4000_5000_5500_5600_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2100_2200_2300_2400_2500_3100_plus), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_1500_3500_3600_7000_8000_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlapsSpan(chr1_500_1500_3500_3600_7000_8000_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_1000_2000_3500_3600_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3500_4500_5500_5600_7000_8000_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_1500_2500_2600_3500_3600_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_2600_3500_3600_3800_4500_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3500_3600_4500_6000_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_3000_3500_3600_7000_8000_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_1000_3000_3500_3600_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3000_4500_5500_5600_7000_8000_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_1500_2500_2600_3500_3600_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2000_2600_3500_3600_3800_4500_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3500_3600_4000_6000_plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_6500_7000_8000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_1500_5500_7000_8000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_1000_7000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_1500_5500_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_5500_6500_6600_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_600_3500_9000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2500_2600_2800_9000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2500_8500_9000_10000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_1500_5500_6000_8000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_1000_1500_7000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_1000_1500_5500_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_5500_6500_7000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_600_2000_9000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2000_2600_2800_9000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2000_8500_9000_10000_plus), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_4500_7000_8000_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_3500_5500_5600_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3500_5500_7000_8000_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3000_6000_7000_8000_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_3500_4500_4600_6500_6600_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_3500_4500_5500_6500_9000_10000_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_4500_5500_6500_7500_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2500_4500_6500_8500_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_5000_7000_8000_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_5000_5500_5600_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3500_5500_6000_8000_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2000_6000_7000_8000_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_4000_4500_4600_6500_6600_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_3500_5000_5500_6500_9000_10000_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_4500_6000_6500_7500_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2000_4500_6500_8500_plus), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_600_1000_2000_plus), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3000_4000_5500_6500_plus), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_2600_4500_4600_5000_6000_plus), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_1500_2500_3500_4500_5000_6000_plus), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_5000_6000_7000_8000_plus), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_2500_3000_4500_4600_5000_6000_plus), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_1500_2500_3000_4500_5000_6000_plus), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_plus), "Same blocks compatible orientation")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlaps(chr1_7000_8000_9000_10000_minus), "Same chromosome, different orientation, non-overlapping spans")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlaps(chr1_1000_4000_7000_8000_minus), "Same chromosome, different orientation, overlapping spans")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlaps(chr1_6000_7000_8000_9000_plus), "Same chromosome, same orientation, adjacent spans")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlaps(chr1_2000_3000_4000_5000_plus), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2000_3000_4000_5000_6000_7000_both), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2000_3000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsSpan(chr1_2000_3000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2000_3000_4000_5000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_900_1000_4000_5000_6000_7000_8000_9000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2500_2600_4500_4600_6500_6600_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlapsSpan(chr1_2500_2600_4500_4600_6500_6600_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_4500_4600_6500_6600_8500_8600_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_1000_2500_2600_6000_6500_8000_9000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2500_3000_8000_9000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_4500_5000_6500_6600_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlaps(chr1_7000_8000_9000_10000_plus), "Same chromosome, same orientation, non-overlapping spans")
      assert(!chr1_1000_2000_3000_4000_5000_6000_plus.overlaps(chr1_6000_8000_9000_10000_plus), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_plus), "Self")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1500_1600_3500_3600_5500_6500_plus), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_600_1500_1600_3500_4500_5500_6500_plus), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1100_1200_1300_1400_3500_3600_plus), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_1500_2500_3500_4500_5500_plus), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_1500_2000_3500_4500_5500_plus), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_1500_2500_3500_4500_5500_6000_7000_plus), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_900_1100_1900_2100_2900_4100_plus), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1500_3500_3600_3700_3800_3900_plus), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_3500_4500_5000_5100_5900_6000_plus), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_900_1100_1900_2100_2900_6000_plus), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1500_3500_3600_3700_3800_5000_plus), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_2000_4500_5000_5100_5900_6000_plus), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3000_3100_3200_3300_4500_4600_plus), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_4500_5100_5200_5300_5400_plus), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_3000_3100_3200_3300_4500_5000_plus), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.overlaps(chr1_500_5000_5100_5200_5300_5400_plus), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_500_1500_3500_4500_plus), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_1500_2400_2600_3500_plus), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_plus.overlaps(chr1_1500_2500_3500_4500_plus), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_500_600_3000_4000_6500_6600_7000_8000_plus), "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1000_2000_2500_6500_7000_8000_plus), "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.overlaps(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus), "Two blocks same")
      assert(chr1_500_600_1500_1600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_1500_1600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_3500_3600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_5500_5600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_2600_3500_3600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_2600_5500_5600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_3500_3600_4500_4600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_3500_3600_6500_6600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_5500_5600_6500_6600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_5500_5600_6500_6600_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_1000_1500_1600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_1500_1600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_3500_3600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_5500_5600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_2000_3000_3500_3600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_3000_5500_5600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_2000_3000_3500_3600_4500_4600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_2000_2600_3500_3600_6500_6600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_2500_3000_5500_5600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_4000_4600_5500_5600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_4000_5000_5500_5600_6000_6600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_5500_5600_6000_6600_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block nested, others non-overlapping")
      assert(chr1_500_600_3500_3600_both.overlaps(chr1_1000_2000_3000_4000_plus), "One block nested, others non-overlapping")
      assert(chr1_1100_1200_2500_3000_both.overlaps(chr1_1000_2000_3000_4000_plus), "One block nested, others non-overlapping")
      assert(chr1_1100_1200_4000_4600_both.overlaps(chr1_1000_2000_3000_4000_plus), "One block nested, others non-overlapping")
      assert(chr1_2000_3000_3500_3600_both.overlaps(chr1_1000_2000_3000_4000_plus), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_900_1100_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_1000_2000_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_900_2000_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping off end of span")
      assert(chr1_500_600_900_1100_both.overlaps(chr1_1000_2000_3000_4000_plus), "One block overlapping off end of span")
      assert(chr1_500_600_1000_2000_both.overlaps(chr1_1000_2000_3000_4000_plus), "One block overlapping off end of span")
      assert(chr1_500_600_900_2000_both.overlaps(chr1_1000_2000_3000_4000_plus), "One block overlapping off end of span")
      assert(chr1_1500_2500_4000_5000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_2500_3500_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_3000_4000_4500_5500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_2500_2600_2700_3500_6000_7000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_500_600_3500_4000_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_500_600_4000_6000_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_500_600_2000_5000_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_3500_5000_6000_7000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_5000_6000_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_500_1000_5000_7000_8000_9000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_4500_4600_5500_6500_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_2000_3000_4000_5000_5500_5600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_2100_2200_2300_2400_2500_3100_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block of other")
      assert(chr1_500_1500_3500_3600_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3500_3600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_4500_5500_5600_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1500_2500_2600_3500_3600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_2500_2600_3500_3600_3800_4500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_3600_4500_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_3000_3500_3600_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_3000_3500_3600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_3000_4500_5500_5600_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1500_2500_2600_3500_3600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_2000_2600_3500_3600_3800_4500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_3600_4000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_6500_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
      assert(chr1_1500_5500_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
      assert(chr1_500_600_1000_7000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
      assert(chr1_500_600_1500_5500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
      assert(chr1_500_5500_6500_6600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(chr1_500_600_3500_9000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(chr1_2500_2600_2800_9000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(chr1_2500_8500_9000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(chr1_1500_5500_6000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
      assert(chr1_500_1000_1500_7000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
      assert(chr1_500_1000_1500_5500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping three blocks of other")
      assert(chr1_500_5500_6500_7000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(chr1_500_600_2000_9000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(chr1_2000_2600_2800_9000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(chr1_2000_8500_9000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping three blocks of other")
      assert(chr1_500_4500_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(chr1_2500_3500_5500_5600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(chr1_3500_5500_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(chr1_3000_6000_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(chr1_500_3500_4500_4600_6500_6600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_3500_4500_5500_6500_9000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_4500_5500_6500_7500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_2500_4500_6500_8500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_500_5000_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(chr1_2500_5000_5500_5600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(chr1_3500_5500_6000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(chr1_2000_6000_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block overlapping two blocks of other")
      assert(chr1_500_4000_4500_4600_6500_6600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_3500_5000_5500_6500_9000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_4500_6000_6500_7500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_2000_4500_6500_8500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "One block overlapping two blocks of other")
      assert(chr1_500_600_1000_2000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(chr1_3000_4000_5500_6500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(chr1_2500_2600_4500_4600_5000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(chr1_1500_2500_3500_4500_5000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(chr1_5000_6000_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(chr1_2500_3000_4500_4600_5000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(chr1_1500_2500_3000_4500_5000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "Same blocks compatible orientation")
      assert(!chr1_7000_8000_9000_10000_minus.overlaps(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, different orientation, non-overlapping spans")
      assert(!chr1_1000_4000_7000_8000_minus.overlaps(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, different orientation, overlapping spans")
      assert(!chr1_6000_7000_8000_9000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, adjacent spans")
      assert(!chr1_6000_7000_8000_9000_plus.overlapsSpan(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, adjacent spans")
      assert(!chr1_2000_3000_4000_5000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(!chr1_2000_3000_4000_5000_6000_7000_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(!chr1_2000_3000_8000_9000_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_2000_3000_4000_5000_8000_9000_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_900_1000_4000_5000_6000_7000_8000_9000_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(!chr1_2500_2600_4500_4600_6500_6600_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(!chr1_4500_4600_6500_6600_8500_8600_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_4500_4600_6500_6600_8500_8600_both.overlapsSpan(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(!chr1_500_1000_2500_2600_6000_6500_8000_9000_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_2500_3000_8000_9000_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_4500_5000_6500_6600_both.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(!chr1_7000_8000_9000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, non-overlapping spans")
      assert(!chr1_6000_8000_9000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_plus), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_plus), "Self")
      assert(chr1_1500_1600_3500_3600_5500_6500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(chr1_1500_1600_3500_3600_5500_6500_plus.overlapsSpan(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(chr1_500_600_1500_1600_3500_4500_5500_6500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in three blocks")
      assert(chr1_1100_1200_1300_1400_3500_3600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in two blocks")
      assert(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in two blocks")
      assert(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in two blocks")
      assert(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in two blocks")
      assert(chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in two blocks")
      assert(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks nested in two blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping three blocks")
      assert(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2000_3500_4500_5500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_6000_7000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping three blocks")
      assert(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping three blocks")
      assert(chr1_900_1100_1900_2100_2900_4100_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(chr1_1500_3500_3600_3700_3800_3900_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(chr1_3500_4500_5000_5100_5900_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(chr1_900_1100_1900_2100_2900_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(chr1_1500_3500_3600_3700_3800_5000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(chr1_2000_4500_5000_5100_5900_6000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Three blocks overlapping two blocks")
      assert(chr1_3000_3100_3200_3300_4500_4600_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "Two blocks nested in one block")
      assert(chr1_500_4500_5100_5200_5300_5400_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "Two blocks nested in one block")
      assert(chr1_3000_3100_3200_3300_4500_5000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "Two blocks nested in one block")
      assert(chr1_500_5000_5100_5200_5300_5400_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_both), "Two blocks nested in one block")
      assert(chr1_500_1500_3500_4500_plus.overlaps(chr1_1000_2000_3000_4000_plus), "Two blocks overlapping two blocks")
      assert(chr1_1500_2400_2600_3500_plus.overlaps(chr1_1000_2000_3000_4000_plus), "Two blocks overlapping two blocks")
      assert(chr1_1500_2500_3500_4500_plus.overlaps(chr1_1000_2000_3000_4000_plus), "Two blocks overlapping two blocks")
      assert(chr1_500_600_3000_4000_6500_6600_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Two blocks same")
      assert(chr1_1000_2000_2500_6500_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Two blocks same")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.overlaps(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both), "Two blocks same")
      assert(!chr1_900_1100_1900_2100_2900_6000_plus.overlaps(chr2_900_1100_1900_2100_2900_6000_plus), "Different chromosomes")
      assert(!chr1_900_1100_1900_2100_2900_6000_plus.overlapsSpan(chr2_900_1100_1900_2100_2900_6000_plus), "Different chromosomes")
    }
  }

}
