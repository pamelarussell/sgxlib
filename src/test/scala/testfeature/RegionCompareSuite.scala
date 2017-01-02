package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class RegionCompareSuite extends FunSuite {

  compareEmpty()
  compareBlockEmpty()
  compareBlockBlock()
  compareBlockBlockSet()
  compareBlockSetEmpty()
  compareBlockSetBlockSet()

  def compareEmpty(): Unit = {
    test("Empty compare") {
      assert(Empty.compare(Empty) === 0)
      assert(Empty.compare(chr1_100_200_300_400_plus) > 0)
      assert(Empty.compare(chr1_1000_2000_unstranded) > 0)
    }
  }

  def compareBlockEmpty(): Unit = {
    test("Block compare Empty") {
      assert(chr1_1000_2000_unstranded.compare(Empty) < 0)
    }
  }

  def compareBlockBlock(): Unit = {
    test("Block compare Block") {
      // Nested blocks
      assert(chr1_1000_2000_both.compare(chr1_1200_1300_plus) < 0)
      assert(chr1_1200_1300_plus.compare(chr1_1000_2000_both) > 0,
        "Block compare containing block should be empty")
      // Nested blocks sharing an endpoint
      assert(chr1_4_6_minus.compare(chr1_5_6_minus) < 0)
      // Overlapping blocks
      assert(chr1_1000_2000_both.compare(chr1_1500_2500_plus) < 0)
      // Non-overlapping blocks
      assert(chr2_1000_2000_plus.compare(chr2_3000_4000_plus) < 0,
        "Block compare non-overlapping block should be original block")
      assert(chr1_1000_2000_minus.compare(chr1_1200_1300_plus) < 0,
        "Block compare non-overlapping block should be original block")
      assert(chr2_1000_2000_plus.compare(chr1_1000_2000_plus_1) > 0,
        "Block compare block on other chromosome")
      // Same span
      assert(chr1_1000_2000_minus.compare(chr1_1000_2000_both) < 0)
    }
  }

  def compareBlockBlockSet(): Unit = {
    test("Block compare BlockSet") {
      // Strictly nested both directions
      assert(chr1_1000_2000_both.compare(chr1_900_2100_3000_4000_plus) > 0,
        "One block of block set completely contains the other block")
      assert(chr1_1000_2000_both.compare(chr1_1100_1200_1300_1400_plus) < 0,
        "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_2000_both.compare(chr1_1000_1100_1200_1300_plus) > 0,
        "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_2000_both.compare(chr1_1000_1100_1900_2000_plus) > 0,
        "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_2000_both.compare(chr1_1000_1100_1200_1300_1900_2000_plus) > 0,
        "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(Block("1", 1000, 1050, Plus).compare(chr1_1000_1100_1200_1300_1900_2000_plus) < 0,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1050, 1100, Plus).compare(chr1_1000_1100_1200_1300_1900_2000_plus) > 0,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1200, 1250, Plus).compare(chr1_1000_1100_1200_1300_1900_2000_plus) > 0,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1950, 2000, Plus).compare(chr1_1000_1100_1200_1300_1900_2000_plus) > 0,
        "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1000_2000_both.compare(chr1_1900_2100_3000_4000_plus) < 0,
        "Overlapping one block off end")
      assert(Block("1", 1650, 1750, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1000_2000_both.compare(chr1_1700_1800_1900_2100_plus) < 0,
        "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1000_2000_both.compare(chr1_1500_1600_2100_2300_plus) < 0,
        "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1000_2000_both.compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(Block("1", 500, 6500, Plus).compare(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus) < 0,
        "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_both.compare(chr1_1000_2000_3000_4000_plus) < 0,
        "Same span as one block")
      assert(Block("1", 1500, 1600, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Same span as one block")
      assert(Block("1", 1700, 1800, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span as one block")
      assert(Block("1", 2100, 2300, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_1000_2000_both.compare(chr1_900_1100_1500_1600_1900_2100_plus) > 0,
        "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(Block("1", 1500, 7500, Plus).compare(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus) > 0,
        "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(Block("1", 2100, 2400, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_1000_2000_plus_1.compare(chr1_100_200_300_400_plus) > 0,
        "Non-overlapping because different spans")
      assert(Block("1", 500, 600, Both).compare(chr1_100_200_300_400_plus) > 0,
        "Non-overlapping because different spans")
      assert(Block("1", 1000, 1100, Both).compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Non-overlapping because different spans")
      assert(Block("1", 2400, 2500, Both).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(chr1_1000_2000_minus.compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Non-overlapping because different orientations")
      // Different chromosomes
      assert(chr2_1000_2000_plus.compare(chr1_2000_3000_plus) > 0,
        "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(Block("1", 200, 300, Plus).compare(chr1_100_200_300_400_plus) > 0,
        "Same span as an intron")
      // Three blocks
      assert(Block("1", 1800, 2100, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span as an intron")
      // Adjacent to span
      assert(Block("1", 400, 500, Plus).compare(chr1_100_200_300_400_plus) > 0,
        "Adjacent to span")
      // Three blocks
      assert(Block("1", 2300, 2400, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(Block("1", 1600, 1699, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "1bp gap between blocks")
      assert(Block("1", 1601, 1700, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "1bp gap between blocks")
      // Outside span
      assert(Block("1", 1400, 1499, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "1bp gap between blocks")
      // Same span different chromosome
      assert(Block("2", 1500, 2300, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span different chromosome")
      // Same span different orientation
      assert(Block("1", 1500, 2300, Unstranded).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span different chromosome")
      // Same span, same chromosome, same orientation
      assert(Block("1", 1500, 2300, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Same span different chromosome")
      // Strictly nested both directions
      assert(chr1_1000_2000_both.compare(chr1_900_2100_3000_4000_plus) > 0,
        "One block of block set completely contains the other block")
      assert(chr1_1000_2000_both.compare(chr1_1100_1200_1300_1400_plus) < 0,
        "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_2000_both.compare(chr1_1000_1100_1200_1300_plus) > 0,
        "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_2000_both.compare(chr1_1000_1100_1900_2000_plus) > 0,
        "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_2000_both.compare(chr1_1000_1100_1200_1300_1900_2000_plus) > 0,
        "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(Block("1", 1000, 1050, Plus).compare(chr1_1000_1100_1200_1300_1900_2000_plus) < 0,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1050, 1100, Plus).compare(chr1_1000_1100_1200_1300_1900_2000_plus) > 0,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1200, 1250, Plus).compare(chr1_1000_1100_1200_1300_1900_2000_plus) > 0,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1950, 2000, Plus).compare(chr1_1000_1100_1200_1300_1900_2000_plus) > 0,
        "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1000_2000_both.compare(chr1_1900_2100_3000_4000_plus) < 0,
        "Overlapping one block off end")
      assert(Block("1", 1650, 1750, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1000_2000_both.compare(chr1_1700_1800_1900_2100_plus) < 0,
        "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1000_2000_both.compare(chr1_1500_1600_2100_2300_plus) < 0,
        "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1000_2000_both.compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(Block("1", 500, 6500, Plus).compare(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus) < 0,
        "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_both.compare(chr1_1000_2000_3000_4000_plus) < 0,
        "Same span as one block")
      assert(Block("1", 1500, 1600, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Same span as one block")
      assert(Block("1", 1700, 1800, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span as one block")
      assert(Block("1", 2100, 2300, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_1000_2000_both.compare(chr1_900_1100_1500_1600_1900_2100_plus) > 0,
        "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(Block("1", 1500, 7500, Plus).compare(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus) > 0,
        "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(Block("1", 2100, 2400, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_1000_2000_plus_1.compare(chr1_100_200_300_400_plus) > 0,
        "Non-overlapping because different spans")
      assert(Block("1", 500, 600, Both).compare(chr1_100_200_300_400_plus) > 0,
        "Non-overlapping because different spans")
      assert(Block("1", 1000, 1100, Both).compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Non-overlapping because different spans")
      assert(Block("1", 2400, 2500, Both).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(chr1_1000_2000_minus.compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Non-overlapping because different orientations")
      // Different chromosomes
      assert(chr2_1000_2000_plus.compare(chr1_2000_3000_plus) > 0,
        "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(Block("1", 200, 300, Plus).compare(chr1_100_200_300_400_plus) > 0,
        "Same span as an intron")
      // Three blocks
      assert(Block("1", 1800, 2100, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span as an intron")
      // Adjacent to span
      assert(Block("1", 400, 500, Plus).compare(chr1_100_200_300_400_plus) > 0,
        "Adjacent to span")
      // Three blocks
      assert(Block("1", 2300, 2400, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(Block("1", 1600, 1699, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "1bp gap between blocks")
      assert(Block("1", 1601, 1700, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "1bp gap between blocks")
      // Outside span
      assert(Block("1", 1400, 1499, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "1bp gap between blocks")
      // Same span different chromosome
      assert(Block("2", 1500, 2300, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span different chromosome")
      // Same span different orientation
      assert(Block("1", 1500, 2300, Unstranded).compare(chr1_1500_1600_1700_1800_2100_2300_plus) > 0,
        "Same span different chromosome")
      // Same span, same chromosome, same orientation
      assert(Block("1", 1500, 2300, Plus).compare(chr1_1500_1600_1700_1800_2100_2300_plus) < 0,
        "Same span different chromosome")
      // Strictly nested both directions
      assert(chr1_900_2100_3000_4000_plus.compare(chr1_1000_2000_both) < 0,
        "One block of block set completely contains the other block")
      assert(chr1_1100_1200_1300_1400_plus.compare(chr1_1000_2000_both) > 0,
        "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_1100_1200_1300_plus.compare(chr1_1000_2000_both) < 0,
        "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_1100_1900_2000_plus.compare(chr1_1000_2000_both) < 0,
        "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.compare(chr1_1000_2000_both) < 0,
        "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.compare(Block("1", 1000, 1050, Plus)) > 0,
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.compare(Block("1", 1050, 1100, Plus)) < 0,
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.compare(Block("1", 1200, 1250, Plus)) < 0,
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.compare(Block("1", 1950, 2000, Plus)) < 0,
        "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1900_2100_3000_4000_plus.compare(chr1_1000_2000_both) > 0,
        "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1650, 1750, Plus)) < 0,
        "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1700_1800_1900_2100_plus.compare(chr1_1000_2000_both) > 0,
        "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1500_1600_2100_2300_plus.compare(chr1_1000_2000_both) > 0,
        "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(chr1_1000_2000_both) > 0,
        "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.compare(Block("1", 500, 6500, Plus)) > 0,
        "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_3000_4000_plus.compare(chr1_1000_2000_both) > 0,
        "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1500, 1600, Plus)) > 0,
        "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1700, 1800, Plus)) < 0,
        "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 2100, 2300, Plus)) < 0,
        "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_900_1100_1500_1600_1900_2100_plus.compare(chr1_1000_2000_both) < 0,
        "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.compare(Block("1", 1500, 7500, Plus)) < 0,
        "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 2100, 2400, Plus)) < 0,
        "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_100_200_300_400_plus.compare(chr1_1000_2000_plus_1) < 0,
        "Non-overlapping because different spans")
      assert(chr1_100_200_300_400_plus.compare(Block("1", 500, 600, Both)) < 0,
        "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1000, 1100, Both)) > 0,
        "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 2400, 2500, Both)) < 0,
        "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(chr1_1000_2000_minus) > 0,
        "Non-overlapping because different orientations")
      // Different chromosomes
      assert(chr1_2000_3000_plus.compare(chr2_1000_2000_plus) < 0,
        "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(chr1_100_200_300_400_plus.compare(Block("1", 200, 300, Plus)) < 0,
        "Same span as an intron")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1800, 2100, Plus)) < 0,
        "Same span as an intron")
      // Adjacent to span
      assert(chr1_100_200_300_400_plus.compare(Block("1", 400, 500, Plus)) < 0,
        "Adjacent to span")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 2300, 2400, Plus)) < 0,
        "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1600, 1699, Plus)) < 0,
        "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1601, 1700, Plus)) < 0,
        "1bp gap between blocks")
      // Outside span
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1400, 1499, Plus)) > 0,
        "1bp gap between blocks")
      // Same span different chromosome
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("2", 1500, 2300, Plus)) < 0,
        "Same span different chromosome")
      // Same span different orientation
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1500, 2300, Unstranded)) < 0,
        "Same span different chromosome")
      // Same span, same chromosome, same orientation
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("1", 1500, 2300, Plus)) > 0,
        "Same span different chromosome")
      // Same span, different chromosome, different orientation
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.compare(Block("2", 1500, 2300, Both)) < 0,
        "Same span different chromosome different orientation")
    }
  }

  def compareBlockSetEmpty(): Unit = {
    test("BlockSet compare Empty") {
      assert(chr1_100_200_300_400_plus.compare(Empty) < 0,
        "Block set compare empty")
    }
  }

  def compareBlockSetBlockSet(): Unit = {
    test("BlockSet compare BlockSet") {

      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_600_1500_1600_plus) > 0,
        "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_600_700_800_5500_5600_plus) > 0,
        "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_2500_2600_3500_3600_4500_4600_plus) < 0,
        "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_5500_5600_6500_6600_7000_8000_plus) < 0,
        "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_600_700_1000_3500_3600_plus) > 0,
        "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_600_2500_3000_5500_5600_plus) > 0,
        "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_2500_3000_5500_5600_plus) < 0,
        "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_5500_5600_6000_6600_7000_8000_plus) < 0,
        "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.compare(chr1_1100_1200_4000_4600_both) < 0,
        "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_600_700_800_900_1100_both) > 0,
        "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_600_700_800_900_2000_both) > 0,
        "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.compare(chr1_500_600_900_1100_both) > 0,
        "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.compare(chr1_500_600_900_2000_both) > 0,
        "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_2500_3500_7000_8000_plus) < 0,
        "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_2500_2600_2700_3500_6000_7000_plus) < 0,
        "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_600_4000_6000_7000_8000_plus) > 0,
        "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_3500_5000_6000_7000_plus) < 0,
        "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_1000_5000_7000_8000_9000_plus) > 0,
        "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_2000_3000_4000_5000_5500_5600_plus) < 0,
        "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_2100_2200_2300_3000_4000_5000_plus) < 0,
        "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_1000_2000_3500_3600_plus) > 0,
        "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_1500_2500_2600_3500_3600_plus) > 0,
        "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus) > 0,
        "One block overlapping one block, another block nested in another block")
       assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.compare(chr1_2000_2600_2800_9000_plus) < 0,
        "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_500_600_1000_2000_plus) > 0,
        "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.compare(chr1_2500_2600_4500_4600_5000_6000_plus) < 0,
        "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.compare(chr1_7000_8000_9000_10000_minus) < 0,
        "Same chromosome, different orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.compare(chr1_1000_4000_7000_8000_minus) < 0,
        "Same chromosome, different orientation, overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.compare(chr1_1000_4000_7000_8000_minus) > 0,
        "Same chromosome, different orientation, overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.compare(chr1_2000_3000_4000_5000_6000_7000_both) < 0,
        "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.compare(chr1_4500_5000_6500_6600_both) < 0,
        "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_plus) === 0,
        "Self")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.compare(chr1_1500_1600_3500_3600_5500_6500_plus) < 0,
        "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.compare(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus) < 0,
        "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.compare(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus) > 0,
        "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.compare(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus) < 0,
        "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.compare(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus) < 0,
        "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_plus.compare(chr1_1500_2500_3500_4500_plus) < 0,
        "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.compare(chr1_1000_2000_2500_6500_7000_8000_plus) > 0,
        "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.compare(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus) > 0,
        "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.compare(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus) < 0,
        "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_both) < 0,
        "Same blocks compatible orientation")
      assert(chr1_7000_8000_9000_10000_minus.compare(chr1_1000_2000_3000_4000_5000_6000_plus) > 0,
        "Same chromosome, different orientation, non-overlapping spans")
      assert(chr1_6000_7000_8000_9000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_plus) > 0,
        "Same chromosome, same orientation, adjacent spans")
      assert(chr1_2000_3000_4000_5000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_plus) > 0,
        "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_plus) === 0,
        "Self")
      assert(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) > 0,
        "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_2500_6500_7000_8000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) < 0,
        "Two blocks same")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) < 0,
        "Two blocks same")
      assert(chr1_900_1100_1900_2100_2900_6000_plus.compare(chr2_900_1100_1900_2100_2900_6000_plus) < 0,
        "Different chromosomes")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_minus) < 0,
        "Same blocks different orientation")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.compare(chr2_1000_2000_3000_4000_5000_6000_plus) < 0,
        "Same blocks different chromosome")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.compare(chr2_1000_2000_3000_4000_5000_6000_minus) < 0,
        "Same blocks different chromosome and orientation")
      assert(chr1_1000_2000_3000_4000_5000_6000_minus.compare(chr1_1000_2000_3000_4000_5000_6000_plus) > 0,
        "Same blocks different orientation")
      assert(chr2_1000_2000_3000_4000_5000_6000_plus.compare(chr1_1000_2000_3000_4000_5000_6000_plus) > 0,
        "Same blocks different chromosome")
      assert(chr2_1000_2000_3000_4000_5000_6000_minus.compare(chr1_1000_2000_3000_4000_5000_6000_plus) > 0,
        "Same blocks different chromosome and orientation")
      // Same span, number of blocks
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.compare(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus) === 0,
        "Same span and number of blocks")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.compare(chr1_1000_2100_2500_3000_4000_5000_6000_6500_7000_8000_plus) < 0,
        "Same span and number of blocks")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.compare(chr1_1000_2000_2400_3000_4000_5000_6000_6500_7000_8000_plus) > 0,
        "Same span and number of blocks")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.compare(chr1_1000_2000_2600_3000_4000_5000_6000_6500_7000_8000_plus) < 0,
        "Same span and number of blocks")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.compare(chr1_1000_2000_4000_5000_6000_6500_6600_6700_7000_8000_plus) < 0,
        "Same span and number of blocks")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.compare(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7100_8000_plus) < 0,
        "Same span and number of blocks")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.compare(chr1_1000_2000_2500_3000_4000_5000_6000_6600_7000_8000_plus) < 0,
        "Same span and number of blocks")
    }
  }



}
