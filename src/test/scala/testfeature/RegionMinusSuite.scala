package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class RegionMinusSuite extends FunSuite {

  minusEmpty()
  minusBlockEmpty()
  minusBlockBlock()
  minusBlockBlockSet()
  minusBlockSetEmpty()
  minusBlockSetBlock()
  minusBlockSetBlockSet()

  def minusEmpty(): Unit = {
    test("Empty minus") {
      assert(Empty.minus(Empty) === Empty, "Empty minus empty should be empty")
      assert(Empty.minus(chr1_100_200_300_400_plus) == Empty, "Empty minus block set should be empty")
      assert(Empty.minus(chr1_1000_2000_both) === Empty, "Empty minus block should be empty")
    }
  }

  def minusBlockEmpty(): Unit = {
    test("Block minus Empty") {
      assert(chr1_1000_2000_both.minus(Empty) === chr1_1000_2000_both,
        "Block minus empty should be original block")
    }
  }

  def minusBlockBlock(): Unit = {
    test("Block minus Block") {
      // Incompatible orientation
      assert(chr1_1200_1300_plus.minus(chr1_1200_1300_minus) === chr1_1200_1300_plus)
      // Nested blocks
      assert(chr1_1000_2000_both.minus(chr1_1200_1300_plus) === BlockSet(
        List(Block("1", 1000, 1200, Unstranded), Block("1", 1300, 2000, Unstranded))
        ))
      assert(chr1_1200_1300_plus.minus(chr1_1000_2000_both) === Empty,
        "Block minus containing block should be empty")
      // Nested blocks sharing an endpoint
      assert(chr1_4_6_minus.minus(chr1_5_6_minus) === Block("1", 4, 5, Minus),
        "Block minus nested block sharing one endpoint should be the rest of the interval")
      // Overlapping blocks
      assert(chr1_1000_2000_both.minus(chr1_1500_2500_plus) === Block("1", 1000, 1500, Unstranded),
        "Block minus overlapping block should be the rest of the interval")
      // Non-overlapping blocks
      assert(chr2_1000_2000_plus.minus(chr2_3000_4000_plus) === chr2_1000_2000_plus,
        "Block minus non-overlapping block should be original block")
      assert(chr1_1000_2000_minus.minus(chr1_1200_1300_plus) === chr1_1000_2000_minus,
        "Block minus non-overlapping block should be original block")
      assert(chr2_1000_2000_plus.minus(chr1_1000_2000_plus_1) === chr2_1000_2000_plus,
        "Block minus block on other chromosome")
      // Same span
      assert(chr1_1000_2000_minus.minus(chr1_1000_2000_both) === Empty,
        "Block minus block with same span should be empty")
      // Equal blocks
      assert(chr1_1000_2000_plus_1.minus(chr1_1000_2000_plus_2) === Empty)
    }
  }

  def minusBlockBlockSet(): Unit = {
    test("Block minus BlockSet") {
      // Strictly nested both directions
      assert(chr1_1000_2000_both.minus(chr1_900_2100_3000_4000_plus) === Empty, "One block of block set completely contains the other block")
      assert(chr1_1000_2000_both.minus(chr1_1100_1200_1300_1400_plus) === BlockSet(List(
        Block("1", 1000, 1100, Unstranded),
        Block("1", 1200, 1300, Unstranded),
        Block("1", 1400, 2000, Unstranded)
      )), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_2000_both.minus(chr1_1000_1100_1200_1300_plus) === BlockSet(List(
        Block("1", 1100, 1200, Unstranded),
        Block("1", 1300, 2000, Unstranded)
      )), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_2000_both.minus(chr1_1000_1100_1900_2000_plus) === Block("1", 1100, 1900, Unstranded),
        "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_2000_both.minus(chr1_1000_1100_1200_1300_1900_2000_plus) === BlockSet(List(
        Block("1", 1100, 1200, Unstranded),
        Block("1", 1300, 1900, Unstranded)
      )), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(Block("1", 1000, 1050, Plus).minus(chr1_1000_1100_1200_1300_1900_2000_plus) === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1050, 1100, Plus).minus(chr1_1000_1100_1200_1300_1900_2000_plus) === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1200, 1250, Plus).minus(chr1_1000_1100_1200_1300_1900_2000_plus) === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1250, 1300, Plus).minus(chr1_1000_1100_1200_1300_1900_2000_plus) === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1900, 1950, Plus).minus(chr1_1000_1100_1200_1300_1900_2000_plus) === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("1", 1950, 2000, Plus).minus(chr1_1000_1100_1200_1300_1900_2000_plus) === Empty,
        "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1000_2000_both.minus(chr1_1900_2100_3000_4000_plus) === Block("1", 1000, 1900, Unstranded),
        "Overlapping one block off end")
      assert(chr1_1500_2500_plus.minus(chr1_1900_2100_3000_4000_plus) === BlockSet(List(
        Block("1", 1500, 1900, Plus),
        Block("1", 2100, 2500, Plus)
      )),
        "Overlapping one block off end")
      assert(Block("1", 1400, 1550, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1400, 1500, Plus), "Overlapping one block off end")
      assert(Block("1", 1550, 1650, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1600, 1650, Plus), "Overlapping one block off end")
      assert(Block("1", 1650, 1750, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1650, 1700, Plus), "Overlapping one block off end")
      assert(Block("1", 1750, 1850, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1800, 1850, Plus), "Overlapping one block off end")
      assert(Block("1", 2000, 2150, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 2000, 2100, Plus), "Overlapping one block off end")
      assert(Block("1", 2200, 2350, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 2300, 2350, Plus), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1000_2000_both.minus(chr1_1700_1800_1900_2100_plus) === BlockSet(List(
        Block("1", 1000, 1700, Unstranded),
        Block("1", 1800, 1900, Unstranded)
      )), "Overlapping two blocks off end")
      assert(Block("1", 1750, 2200, Plus).minus(chr1_1700_1800_1900_2100_plus) === BlockSet(List(
        Block("1", 1800, 1900, Plus),
        Block("1", 2100, 2200, Plus)
      )), "Overlapping two blocks off end")
      assert(Block("1", 1400, 1750, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) === BlockSet(List(
        Block("1", 1400, 1500, Plus),
        Block("1", 1600, 1700, Plus)
      )), "Overlapping two blocks off end")
      assert(Block("1", 1550, 1850, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) === BlockSet(List(
        Block("1", 1600, 1700, Plus),
        Block("1", 1800, 1850, Plus)
      )), "Overlapping two blocks off end")
      assert(Block("1", 1650, 2200, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) === BlockSet(List(
        Block("1", 1650, 1700, Plus),
        Block("1", 1800, 2100, Plus)
      )), "Overlapping two blocks off end")
      assert(Block("1", 1750, 2400, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) === BlockSet(List(
        Block("1", 1800, 2100, Plus),
        Block("1", 2300, 2400, Plus)
      )), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1000_2000_both.minus(chr1_1500_1600_2100_2300_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded)
      )), "Overlapping one block and an intron")
      assert(chr1_2000_3000_plus.minus(chr1_1500_1600_2100_2300_plus) === BlockSet(List(
        Block("1", 2000, 2100, Plus),
        Block("1", 2300, 3000, Plus)
      )), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1000_2000_both.minus(chr1_1500_1600_1700_1800_2100_2300_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 1700, Unstranded),
        Block("1", 1800, 2000, Unstranded)
      )), "Overlapping two blocks and an intron")
      assert(Block("1", 1650, 2400, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) === BlockSet(List(
        Block("1", 1650, 1700, Plus),
        Block("1", 1800, 2100, Plus),
        Block("1", 2300, 2400, Plus)
      )), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(Block("1", 500, 6500, Plus).minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 6500, Plus)
      )), "Overlapping three blocks and an intron")
      assert(Block("1", 2500, 8500, Plus).minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus),
        Block("1", 8000, 8500, Plus)
      )), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_both.minus(chr1_1000_2000_3000_4000_plus) === Empty, "Same span as one block")
      assert(Block("1", 1500, 1600, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) === Empty, "Same span as one block")
      assert(Block("1", 1700, 1800, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) === Empty, "Same span as one block")
      assert(Block("1", 2100, 2300, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) === Empty, "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_1000_2000_both.minus(chr1_900_1100_1500_1600_1900_2100_plus) === BlockSet(List(
        Block("1", 1100, 1500, Unstranded),
        Block("1", 1600, 1900, Unstranded)
      )), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(Block("1", 1500, 7500, Plus).minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus)
      )), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(Block("1", 1400, 1600, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1400, 1500, Plus), "Contains one block sharing an endpoint")
      assert(Block("1", 1500, 1650, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1600, 1650, Plus), "Contains one block sharing an endpoint")
      assert(Block("1", 1650, 1800, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1650, 1700, Plus), "Contains one block sharing an endpoint")
      assert(Block("1", 1700, 1850, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1800, 1850, Plus), "Contains one block sharing an endpoint")
      assert(Block("1", 2000, 2300, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 2000, 2100, Plus), "Contains one block sharing an endpoint")
      assert(Block("1", 2100, 2400, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 2300, 2400, Plus), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_1000_2000_plus_1.minus(chr1_100_200_300_400_plus) ===
        chr1_1000_2000_plus_1, "Non-overlapping because different spans")
      assert(Block("1", 500, 600, Unstranded).minus(chr1_100_200_300_400_plus) ===
        Block("1", 500, 600, Unstranded), "Non-overlapping because different spans")
      assert(Block("1", 1000, 1100, Unstranded).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1000, 1100, Unstranded), "Non-overlapping because different spans")
      assert(Block("1", 2400, 2500, Unstranded).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 2400, 2500, Unstranded), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(chr1_1000_2000_minus.minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        chr1_1000_2000_minus, "Non-overlapping because different orientations")
      // Different chromosomes
      assert(chr2_1000_2000_plus.minus(chr1_2000_3000_plus) ===
        chr2_1000_2000_plus, "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(Block("1", 200, 300, Plus).minus(chr1_100_200_300_400_plus) ===
        Block("1", 200, 300, Plus), "Same span as an intron")
      // Three blocks
      assert(Block("1", 1600, 1700, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1600, 1700, Plus), "Same span as an intron")
      assert(Block("1", 1800, 2100, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1800, 2100, Plus), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(Block("1", 50, 100, Plus).minus(chr1_100_200_300_400_plus) ===
        Block("1", 50, 100, Plus), "Adjacent to span")
      assert(Block("1", 400, 500, Plus).minus(chr1_100_200_300_400_plus) ===
        Block("1", 400, 500, Plus), "Adjacent to span")
      // Three blocks
      assert(Block("1", 1400, 1500, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1400, 1500, Plus), "Same span as an intron")
      assert(Block("1", 2300, 2400, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 2300, 2400, Plus), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(Block("1", 201, 300, Plus).minus(chr1_100_200_300_400_plus) ===
        Block("1", 201, 300, Plus), "1bp gap between blocks")
      assert(Block("1", 200, 299, Plus).minus(chr1_100_200_300_400_plus) ===
        Block("1", 200, 299, Plus), "1bp gap between blocks")
      assert(Block("1", 201, 299, Plus).minus(chr1_100_200_300_400_plus) ===
        Block("1", 201, 299, Plus), "1bp gap between blocks")
      assert(Block("1", 1801, 2099, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1801, 2099, Plus), "1bp gap between blocks")
      assert(Block("1", 1601, 1699, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1601, 1699, Plus), "1bp gap between blocks")
      assert(Block("1", 1801, 2099, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1801, 2099, Plus), "1bp gap between blocks")
      assert(Block("1", 1600, 1699, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1600, 1699, Plus), "1bp gap between blocks")
      assert(Block("1", 1800, 2099, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1800, 2099, Plus), "1bp gap between blocks")
      assert(Block("1", 1601, 1700, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1601, 1700, Plus), "1bp gap between blocks")
      // Outside span
      assert(Block("1", 1400, 1499, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1400, 1499, Plus), "1bp gap between blocks")
      assert(Block("1", 2301, 2400, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 2301, 2400, Plus), "1bp gap between blocks")
      assert(Block("1", 1400, 1500, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 1400, 1500, Plus), "1bp gap between blocks")
      assert(Block("1", 2300, 2400, Plus).minus(chr1_1500_1600_1700_1800_2100_2300_plus) ===
        Block("1", 2300, 2400, Plus), "1bp gap between blocks")
    }
  }

  def minusBlockSetEmpty(): Unit = {
    test("BlockSet minus Empty") {
      assert(chr1_100_200_300_400_plus.minus(Empty) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "Block set minus empty")
    }
  }

  def minusBlockSetBlock(): Unit = {
    test("BlockSet minus Block") {
      // Strictly nested both directions
      assert(chr1_900_2100_3000_4000_plus.minus(chr1_1000_2000_both) === BlockSet(List(
        Block("1", 900, 1000, Plus),
        Block("1", 2000, 2100, Plus),
        Block("1", 3000, 4000, Plus)
      )), "One block of block set completely contains the other block")
      assert(chr1_1100_1200_1300_1400_plus.minus(chr1_1000_2000_both) === Empty, "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_1100_1200_1300_plus.minus(chr1_1000_2000_both) === Empty, "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_1100_1900_2000_plus.minus(chr1_1000_2000_both) === Empty, "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(chr1_1000_2000_both) === Empty,
        "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("1", 1000, 1050, Plus)) === BlockSet(List(
        Block("1", 1050, 1100, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1900, 2000, Plus)
      )),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("1", 1050, 1100, Plus)) === BlockSet(List(
        Block("1", 1000, 1050, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1900, 2000, Plus)
      )),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("1", 1200, 1250, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1250, 1300, Plus),
        Block("1", 1900, 2000, Plus)
      )),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("1", 1250, 1300, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 1250, Plus),
        Block("1", 1900, 2000, Plus)
      )),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("1", 1900, 1950, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1950, 2000, Plus)
      )),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("1", 1950, 2000, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1900, 1950, Plus)
      )),
        "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1900_2100_3000_4000_plus.minus(chr1_1000_2000_both) === BlockSet(List(
        Block("1", 2000, 2100, Plus),
        Block("1", 3000, 4000, Plus)
      )),
        "Overlapping one block off end")
      assert(chr1_1900_2100_3000_4000_plus.minus(chr1_1500_2500_plus) === Block("1", 3000, 4000, Plus),
        "Overlapping one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1400, 1550, Plus)) === BlockSet(List(
        Block("1", 1550, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1550, 1650, Plus)) === BlockSet(List(
        Block("1", 1500, 1550, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1650, 1750, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1750, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1750, 1850, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1750, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 2000, 2150, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2150, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 2200, 2350, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2200, Plus)
      )), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1700_1800_1900_2100_plus.minus(chr1_1000_2000_both) === Block("1", 2000, 2100, Plus),
        "Overlapping two blocks off end")
      assert(chr1_1700_1800_1900_2100_plus.minus(Block("1", 1750, 2200, Plus)) === Block("1", 1700, 1750, Plus),
        "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1400, 1750, Plus)) === BlockSet(List(
        Block("1", 1750, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1550, 1850, Plus)) === BlockSet(List(
        Block("1", 1500, 1550, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1650, 2200, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 2200, 2300, Plus)
      )), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1750, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1750, Plus)
      )), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1500_1600_2100_2300_plus.minus(chr1_1000_2000_both) === Block("1", 2100, 2300, Plus),
        "Overlapping one block and an intron")
      assert(chr1_1500_1600_2100_2300_plus.minus(chr1_2000_3000_plus) === Block("1", 1500, 1600, Plus),
        "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(chr1_1000_2000_both) === Block("1", 2100, 2300, Plus),
        "Overlapping two blocks and an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1650, 2400, Plus)) ===
        Block("1", 1500, 1600, Plus), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.minus(Block("1", 500, 6500, Plus)) ===
        Block("1", 7000, 8000, Plus), "Overlapping three blocks and an intron")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.minus(Block("1", 2500, 8500, Plus)) ===
        Block("1", 1000, 2000, Plus), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1000_2000_both) ===
        Block("1", 3000, 4000, Plus), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1500, 1600, Plus)) === BlockSet(List(
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1700, 1800, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 2100, 2300, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus)
      )), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_900_1100_1500_1600_1900_2100_plus.minus(chr1_1000_2000_both) === BlockSet(List(
        Block("1", 900, 1000, Plus),
        Block("1", 2000, 2100, Plus)
      )), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.minus(Block("1", 1500, 7500, Plus)) === BlockSet(List(
        Block("1", 1000, 1500, Plus),
        Block("1", 7500, 8000, Plus)
      )), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1400, 1600, Plus)) === BlockSet(List(
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1500, 1650, Plus)) === BlockSet(List(
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1650, 1800, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1700, 1850, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 2000, 2300, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 2100, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus)
      )), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_100_200_300_400_plus.minus(chr1_1000_2000_plus_1) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "Non-overlapping because different spans")
      assert(chr1_100_200_300_400_plus.minus(Block("1", 500, 600, Unstranded)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1000, 1100, Unstranded)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 2400, 2500, Unstranded)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(chr1_1000_2000_minus) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Non-overlapping because different orientations")
      // Different chromosomes
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.minus(chr2_1000_2000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Minus),
        Block("1", 3000, 4000, Minus),
        Block("1", 5000, 6000, Minus),
        Block("1", 7000, 8000, Minus)
      )), "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(chr1_100_200_300_400_plus.minus(Block("1", 200, 300, Plus)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "Same span as an intron")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1600, 1700, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1800, 2100, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(chr1_100_200_300_400_plus.minus(Block("1", 50, 100, Plus)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "Adjacent to span")
      assert(chr1_100_200_300_400_plus.minus(Block("1", 400, 500, Plus)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "Adjacent to span")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1400, 1500, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 2300, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(chr1_100_200_300_400_plus.minus(Block("1", 201, 300, Plus)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.minus(Block("1", 200, 299, Plus)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.minus(Block("1", 201, 299, Plus)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1801, 2099, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1601, 1699, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1801, 2099, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1600, 1699, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1800, 2099, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1601, 1700, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      // Outside span
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1400, 1499, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 2301, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 1400, 1500, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("1", 2300, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
    }
  }

  def minusBlockSetBlockSet(): Unit = {
    test("BlockSet minus BlockSet") {

      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1500_1600_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_1500_1600_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_3500_3600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_5500_5600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2500_2600_3500_3600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2500_2600_5500_5600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_3500_3600_4500_4600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_3500_3600_6500_6600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_5500_5600_6500_6600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_5500_5600_6500_6600_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_1500_1600_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_1000_1500_1600_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_1000_3500_3600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_1000_5500_5600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2000_3000_3500_3600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2500_3000_5500_5600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_3000_3500_3600_4500_4600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_2600_3500_3600_6500_6600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_3000_5500_5600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_4000_4600_5500_5600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_4000_5000_5500_5600_6000_6600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_5500_5600_6000_6600_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_600_3500_3600_both) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 3000, 3500, Plus),
        Block("1", 3600, 4000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1100_1200_2500_3000_both) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 2000, Plus),
        Block("1", 3000, 4000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1100_1200_4000_4600_both) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 2000, Plus),
        Block("1", 3000, 4000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_2000_3000_3500_3600_both) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 3000, 3500, Plus),
        Block("1", 3600, 4000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_900_1100_both) === BlockSet(List(
        Block("1", 1100, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_1000_2000_both) === BlockSet(List(
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_900_2000_both) === BlockSet(List(
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
     )), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_600_900_1100_both) === BlockSet(List(
        Block("1", 1100, 2000, Plus),
        Block("1", 3000, 4000, Plus)
      )), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_600_1000_2000_both) ===
        Block("1", 3000, 4000, Plus), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_600_900_2000_both) ===
        Block("1", 3000, 4000, Plus), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_2500_4000_5000_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_3500_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3500, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_4000_4500_5500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 5500, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_2700_3500_6000_7000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3500, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_3500_4000_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_4000_6000_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2000_5000_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_5000_6000_7000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_5000_6000_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_5000_7000_8000_9000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_4500_4600_5500_6500_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_3000_4000_5000_5500_5600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2100_2200_2300_2400_2500_3100_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3100, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2100_2200_2300_3000_4000_5000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1500_3500_3600_7000_8000_plus) === BlockSet(List(
        Block("1", 1500, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1000_2000_3500_3600_plus) === BlockSet(List(
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_4500_5500_5600_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1500_2500_2600_3500_3600_plus) === BlockSet(List(
        Block("1", 1500, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_3500_3600_3800_4500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 3800, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_3600_4500_6000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_3000_3500_3600_7000_8000_plus) === BlockSet(List(
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1000_3000_3500_3600_plus) === BlockSet(List(
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_4500_5500_5600_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1500_2500_2600_3500_3600_plus) === BlockSet(List(
        Block("1", 1500, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_2600_3500_3600_3800_4500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 3800, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_3600_4000_6000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_6500_7000_8000_plus) ===
        Empty, "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_5500_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 5500, 6000, Unstranded)
      )), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1000_7000_plus) ===
        Empty, "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1500_5500_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 5500, 6000, Unstranded)
      )), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_5500_6500_6600_plus) === BlockSet(List(
        Block("1", 5500, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_3500_9000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded)
      )), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_2600_2800_9000_plus) ===
        Block("1", 1000, 2000, Unstranded), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_8500_9000_10000_plus) ===
        Block("1", 1000, 2000, Unstranded), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_5500_6000_8000_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 5500, 6000, Unstranded)
      )), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_1500_7000_plus) ===
        Block("1", 1000, 1500, Unstranded), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_1500_5500_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 5500, 6000, Unstranded)
      )), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_5500_6500_7000_plus) === BlockSet(List(
        Block("1", 5500, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_2000_9000_plus) ===
        Block("1", 1000, 2000, Unstranded), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_2600_2800_9000_plus) ===
        Block("1", 1000, 2000, Unstranded), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_8500_9000_10000_plus) ===
        Block("1", 1000, 2000, Unstranded), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_4500_7000_8000_plus) ===
        Block("1", 5000, 6000, Unstranded), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_3500_5500_5600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3500, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_5500_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5500, 6000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_6000_7000_8000_plus) ===
        Block("1", 1000, 2000, Unstranded), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_3500_4500_4600_6500_6600_plus) === BlockSet(List(
        Block("1", 3500, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5500, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_4500_5500_6500_9000_10000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_4500_5500_6500_7500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5500, 6000, Unstranded),
        Block("1", 7500, 8000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_4500_6500_8500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_5000_7000_8000_plus) ===
        Block("1", 5000, 6000, Unstranded), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_5000_5500_5600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_5500_6000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5500, 6000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_6000_7000_8000_plus) ===
        Block("1", 1000, 2000, Unstranded), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_4000_4500_4600_6500_6600_plus) === BlockSet(List(
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5500, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_5000_5500_6500_9000_10000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_4500_6000_6500_7500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 7500, 8000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_4500_6500_8500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1000_2000_plus) === BlockSet(List(
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_4000_5500_6500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 5000, 5500, Unstranded)
      )), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_4500_4600_5000_6000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded)
      )), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_2500_3500_4500_5000_6000_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 3000, 3500, Unstranded)
      )), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_5000_6000_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded)
      )), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_3000_4500_4600_5000_6000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded)
      )), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_2500_3000_4500_5000_6000_plus) ===
        Block("1", 1000, 1500, Unstranded), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1000_2000_3000_4000_5000_6000_plus) ===
        Empty, "Same blocks compatible orientation")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_7000_8000_9000_10000_minus) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 3000, 4000, Plus),
        Block("1", 5000, 6000, Plus)
      )), "Same chromosome, different orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_1000_4000_7000_8000_minus) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 3000, 4000, Plus),
        Block("1", 5000, 6000, Plus)
      )), "Same chromosome, different orientation, overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_6000_7000_8000_9000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 3000, 4000, Plus),
        Block("1", 5000, 6000, Plus)
      )), "Same chromosome, same orientation, adjacent spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_2000_3000_4000_5000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 3000, 4000, Plus),
        Block("1", 5000, 6000, Plus)
      )), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_3000_4000_5000_6000_7000_both) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_3000_8000_9000_both) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_3000_4000_5000_8000_9000_both) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_900_1000_4000_5000_6000_7000_8000_9000_both) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_2600_4500_4600_6500_6600_both) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_4500_4600_6500_6600_8500_8600_both) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1000_2500_2600_6000_6500_8000_9000_both) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_3000_8000_9000_both) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_4500_5000_6500_6600_both) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_7000_8000_9000_10000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 3000, 4000, Plus),
        Block("1", 5000, 6000, Plus)
      )), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_6000_8000_9000_10000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 3000, 4000, Plus),
        Block("1", 5000, 6000, Plus)
      )), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus) ===
        Empty, "Self")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_3500_3600_5500_6500_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_1500_1600_3500_4500_5500_6500_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded),
        Block("1", 7000, 7500, Unstranded),
        Block("1", 7600, 8000, Unstranded)
      )), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 7500, Unstranded),
        Block("1", 7600, 8000, Unstranded)
      )), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 5500, Unstranded),
        Block("1", 5600, 6000, Unstranded),
        Block("1", 7000, 7500, Unstranded),
        Block("1", 7600, 8000, Unstranded)
      )), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 7500, Unstranded),
        Block("1", 7600, 8000, Unstranded)
      )), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1100_1200_1300_1400_3500_3600_plus) === BlockSet(List(
        Block("1", 1000, 1100, Unstranded),
        Block("1", 1200, 1300, Unstranded),
        Block("1", 1400, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 5100, Unstranded),
        Block("1", 5200, 5300, Unstranded),
        Block("1", 5400, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 7000, 7500, Unstranded),
        Block("1", 7600, 7700, Unstranded),
        Block("1", 7800, 8000, Unstranded)
      )), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus) === BlockSet(List(
        Block("1", 1000, 1100, Unstranded),
        Block("1", 1200, 1300, Unstranded),
        Block("1", 1400, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 3600, 4000, Unstranded),
        Block("1", 5000, 5100, Unstranded),
        Block("1", 5200, 5300, Unstranded),
        Block("1", 5400, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 1600, 2000, Unstranded),
        Block("1", 7000, 7500, Unstranded),
        Block("1", 7600, 7700, Unstranded),
        Block("1", 7800, 8000, Unstranded)
      )), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1500_2500_3500_4500_5500_plus) === BlockSet(List(
        Block("1", 1500, 2000, Unstranded),
        Block("1", 3500, 4000, Unstranded),
        Block("1", 5500, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus) === BlockSet(List(
        Block("1", 1500, 2000, Unstranded),
        Block("1", 3500, 4000, Unstranded),
        Block("1", 5500, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus) === BlockSet(List(
        Block("1", 1100, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 7500, Unstranded)
      )), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1500_2000_3500_4500_5500_plus) === BlockSet(List(
        Block("1", 1500, 2000, Unstranded),
        Block("1", 3500, 4000, Unstranded),
        Block("1", 5500, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1500_2500_3500_4500_5500_6000_7000_plus) === BlockSet(List(
        Block("1", 1500, 2000, Unstranded),
        Block("1", 3500, 4000, Unstranded),
        Block("1", 5500, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus) === BlockSet(List(
        Block("1", 1100, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 7500, Unstranded)
      )), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_900_1100_1900_2100_2900_4100_plus) === BlockSet(List(
        Block("1", 1100, 1900, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_3500_3600_3700_3800_3900_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 3500, 3600, Unstranded),
        Block("1", 3700, 3800, Unstranded),
        Block("1", 3900, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_4500_5000_5100_5900_6000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3000, 3500, Unstranded),
        Block("1", 5100, 5900, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus) === BlockSet(List(
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7100, 7500, Unstranded)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_900_1100_1900_2100_2900_6000_plus) === BlockSet(List(
        Block("1", 1100, 1900, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_3500_3600_3700_3800_5000_plus) === BlockSet(List(
        Block("1", 1000, 1500, Unstranded),
        Block("1", 3500, 3600, Unstranded),
        Block("1", 3700, 3800, Unstranded),
        Block("1", 5000, 6000, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_4500_5000_5100_5900_6000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 5100, 5900, Unstranded),
        Block("1", 7000, 8000, Unstranded)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus) === BlockSet(List(
          Block("1", 3000, 4000, Unstranded),
          Block("1", 5000, 6000, Unstranded),
          Block("1", 7100, 7500, Unstranded)
        )), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_3100_3200_3300_4500_4600_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3100, 3200, Unstranded),
        Block("1", 3300, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_4500_5100_5200_5300_5400_plus) === BlockSet(List(
        Block("1", 5000, 5100, Unstranded),
        Block("1", 5200, 5300, Unstranded),
        Block("1", 5400, 6000, Unstranded)
      )), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_3100_3200_3300_4500_5000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 3100, 3200, Unstranded),
        Block("1", 3300, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_5000_5100_5200_5300_5400_plus) === BlockSet(List(
        Block("1", 5000, 5100, Unstranded),
        Block("1", 5200, 5300, Unstranded),
        Block("1", 5400, 6000, Unstranded)
      )), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_1500_3500_4500_plus) === BlockSet(List(
        Block("1", 1500, 2000, Plus),
        Block("1", 3000, 3500, Plus)
      )), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1500_2400_2600_3500_plus) === BlockSet(List(
        Block("1", 1000, 1500, Plus),
        Block("1", 3500, 4000, Plus)
      )), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1500_2500_3500_4500_plus) === BlockSet(List(
        Block("1", 1000, 1500, Plus),
        Block("1", 3000, 3500, Plus)
      )), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_3000_4000_6500_6600_7000_8000_plus) === BlockSet(List(
        Block("1", 1000, 2000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1000_2000_2500_6500_7000_8000_plus) === Empty, "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus) === BlockSet(List(
        Block("1", 3000, 4000, Unstranded),
        Block("1", 5000, 6000, Unstranded)
      )), "Two blocks same")
      assert(chr1_500_600_1500_1600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 500, 600, Plus), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_1500_1600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 700, 800, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 700, 800, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 700, 800, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_2600_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 2500, 2600, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_2600_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 2500, 2600, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_3500_3600_4500_4600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2500, 2600, Plus),
        Block("1", 4500, 4600, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_3500_3600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2500, 2600, Plus),
        Block("1", 6500, 6600, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_5500_5600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2500, 2600, Plus),
        Block("1", 6500, 6600, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_5500_5600_6500_6600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 6500, 6600, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_1000_1500_1600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 500, 1000, Plus), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_1500_1600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 700, 1000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 700, 1000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 700, 1000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_600_2000_3000_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 2000, 3000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_3000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 2500, 3000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_2000_3000_3500_3600_4500_4600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4500, 4600, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_2000_2600_3500_3600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 2600, Plus),
        Block("1", 6500, 6600, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_2500_3000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 2500, 3000, Plus), "One block nested, others non-overlapping")
      assert(chr1_4000_4600_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 4000, 4600, Plus), "One block nested, others non-overlapping")
      assert(chr1_4000_5000_5500_5600_6000_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 6600, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_5500_5600_6000_6600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 6000, 6600, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block nested, others non-overlapping")
      assert(chr1_500_600_3500_3600_both.minus(chr1_1000_2000_3000_4000_plus) ===
        Block("1", 500, 600, Unstranded), "One block nested, others non-overlapping")
      assert(chr1_1100_1200_2500_3000_both.minus(chr1_1000_2000_3000_4000_plus) ===
        Block("1", 2500, 3000, Unstranded), "One block nested, others non-overlapping")
      assert(chr1_1100_1200_4000_4600_both.minus(chr1_1000_2000_3000_4000_plus) ===
        Block("1", 4000, 4600, Unstranded), "One block nested, others non-overlapping")
      assert(chr1_2000_3000_3500_3600_both.minus(chr1_1000_2000_3000_4000_plus) ===
        Block("1", 2000, 3000, Unstranded), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_900_1100_both.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Unstranded),
        Block("1", 700, 800, Unstranded),
        Block("1", 900, 1000, Unstranded)
      )), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_1000_2000_both.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Unstranded),
        Block("1", 700, 800, Unstranded)
      )), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_900_2000_both.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Unstranded),
        Block("1", 700, 800, Unstranded),
        Block("1", 900, 1000, Unstranded)
      )), "One block overlapping off end of span")
      assert(chr1_500_600_900_1100_both.minus(chr1_1000_2000_3000_4000_plus) === BlockSet(List(
        Block("1", 500, 600, Unstranded),
        Block("1", 900, 1000, Unstranded)
      )), "One block overlapping off end of span")
      assert(chr1_500_600_1000_2000_both.minus(chr1_1000_2000_3000_4000_plus) ===
        Block("1", 500, 600, Unstranded), "One block overlapping off end of span")
      assert(chr1_500_600_900_2000_both.minus(chr1_1000_2000_3000_4000_plus) === BlockSet(List(
        Block("1", 500, 600, Unstranded),
        Block("1", 900, 1000, Unstranded)
      )), "One block overlapping off end of span")
      assert(chr1_1500_2500_4000_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 2500, Plus),
        Block("1", 4000, 5000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_2500_3500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_3000_4000_4500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 4500, 5000, Plus), "One block overlapping one block of other")
      assert(chr1_2500_2600_2700_3500_6000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2500, 2600, Plus),
        Block("1", 2700, 3000, Plus),
        Block("1", 6000, 7000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_500_600_3500_4000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_500_600_4000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_500_600_2000_5000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_3500_5000_6000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_5000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 7000, 8000, Plus), "One block overlapping one block of other")
      assert(chr1_500_1000_5000_7000_8000_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 6000, 7000, Plus),
        Block("1", 8000, 9000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_4500_4600_5500_6500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 4500, 4600, Plus),
        Block("1", 6000, 6500, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_2000_3000_4000_5000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_2100_2200_2300_2400_2500_3100_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2100, 2200, Plus),
        Block("1", 2300, 2400, Plus),
        Block("1", 2500, 3000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_2100_2200_2300_3000_4000_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2100, 2200, Plus),
        Block("1", 2300, 3000, Plus),
        Block("1", 4000, 5000, Plus)
      )), "One block overlapping one block of other")
      assert(chr1_500_1500_3500_3600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === Empty,
        "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_4500_5500_5600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 4000, 4500, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1500_2500_2600_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2500, 2600, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_2500_2600_3500_3600_3800_4500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2500, 2600, Plus),
        Block("1", 4000, 4500, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 2500, 2600, Plus),
        Block("1", 4500, 4600, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_3600_4500_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 4500, 5000, Plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_3000_3500_3600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_3000_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 2000, 3000, Plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_3000_4500_5500_5600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 4000, 4500, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1500_2500_2600_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2500, 2600, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_2000_2600_3500_3600_3800_4500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 2600, Plus),
        Block("1", 4000, 4500, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2500, 2600, Plus),
        Block("1", 4500, 4600, Plus)
      )), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_3600_4000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 4000, 5000, Plus), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_6500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 6500, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_1500_5500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_500_600_1000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_500_600_1500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_500_5500_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6500, 6600, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_500_600_3500_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus),
        Block("1", 8000, 9000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_2500_2600_2800_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 2600, Plus),
        Block("1", 2800, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus),
        Block("1", 8000, 9000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_2500_8500_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus),
        Block("1", 8000, 8500, Plus),
        Block("1", 9000, 10000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_1500_5500_6000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 8000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_500_1000_1500_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_500_1000_1500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_500_5500_6500_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6500, 7000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_500_600_2000_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus),
        Block("1", 8000, 9000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_2000_2600_2800_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 2600, Plus),
        Block("1", 2800, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus),
        Block("1", 8000, 9000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_2000_8500_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus),
        Block("1", 8000, 8500, Plus),
        Block("1", 9000, 10000, Plus)
      )), "One block overlapping three blocks of other")
      assert(chr1_500_4500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 4500, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_2500_3500_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 2500, 3000, Plus), "One block overlapping two blocks of other")
      assert(chr1_3500_5500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 4000, 5000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_3000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 4000, 5000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_500_3500_4500_4600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4500, 4600, Plus),
        Block("1", 6500, 6600, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 2600, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6500, 6600, Plus),
        Block("1", 8500, 8600, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_3500_4500_5500_6500_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 4000, 4500, Plus),
        Block("1", 6000, 6500, Plus),
        Block("1", 9000, 10000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_4500_5500_6500_7500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 4500, 5000, Plus),
        Block("1", 6500, 7000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_2500_4500_6500_8500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 4500, Plus),
        Block("1", 6500, 7000, Plus),
        Block("1", 8000, 8500, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_500_5000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_2500_5000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 5000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_3500_5500_6000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 8000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_2000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 7000, 8000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_500_4000_4500_4600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4500, 4600, Plus),
        Block("1", 6500, 6600, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6500, 6600, Plus),
        Block("1", 8500, 8600, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_3500_5000_5500_6500_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 6500, Plus),
        Block("1", 9000, 10000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_4500_6000_6500_7500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 4500, 5000, Plus),
        Block("1", 6500, 7000, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_2000_4500_6500_8500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 4500, Plus),
        Block("1", 6500, 7000, Plus),
        Block("1", 8000, 8500, Plus)
      )), "One block overlapping two blocks of other")
      assert(chr1_500_600_1000_2000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 500, 600, Plus), "One block same")
      assert(chr1_3000_4000_5500_6500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 6000, 6500, Plus), "One block same")
      assert(chr1_2500_2600_4500_4600_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2500, 2600, Plus),
        Block("1", 4500, 4600, Plus)
      )), "One block same")
      assert(chr1_1500_2500_3500_4500_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 2500, Plus),
        Block("1", 4000, 4500, Plus)
      )), "One block same")
      assert(chr1_5000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 7000, 8000, Plus), "One block same")
      assert(chr1_2500_3000_4500_4600_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4500, 4600, Plus)
      )), "One block same")
      assert(chr1_1500_2500_3000_4500_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 2000, 2500, Plus),
        Block("1", 4000, 4500, Plus)
      )), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Empty, "Same blocks compatible orientation")
      assert(chr1_7000_8000_9000_10000_minus.minus(chr1_1000_2000_3000_4000_5000_6000_plus) === BlockSet(List(
        Block("1", 7000, 8000, Minus),
        Block("1", 9000, 10000, Minus)
      )), "Same chromosome, different orientation, non-overlapping spans")
      assert(chr1_1000_4000_7000_8000_minus.minus(chr1_1000_2000_3000_4000_5000_6000_plus) === BlockSet(List(
        Block("1", 1000, 4000, Minus),
        Block("1", 7000, 8000, Minus)
      )), "Same chromosome, different orientation, overlapping spans")
      assert(chr1_6000_7000_8000_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus) === BlockSet(List(
        Block("1", 6000, 7000, Plus),
        Block("1", 8000, 9000, Plus)
      )), "Same chromosome, same orientation, adjacent spans")
      assert(chr1_2000_3000_4000_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus)
      )), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_2000_3000_4000_5000_6000_7000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 3000, Unstranded),
        Block("1", 4000, 5000, Unstranded),
        Block("1", 6000, 7000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_2000_3000_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 3000, Unstranded),
        Block("1", 8000, 9000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_2000_3000_4000_5000_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 3000, Unstranded),
        Block("1", 4000, 5000, Unstranded),
        Block("1", 8000, 9000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_900_1000_4000_5000_6000_7000_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 900, 1000, Unstranded),
        Block("1", 4000, 5000, Unstranded),
        Block("1", 6000, 7000, Unstranded),
        Block("1", 8000, 9000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_2500_2600_4500_4600_6500_6600_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 2600, Unstranded),
        Block("1", 4500, 4600, Unstranded),
        Block("1", 6500, 6600, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_4500_4600_6500_6600_8500_8600_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 4500, 4600, Unstranded),
        Block("1", 6500, 6600, Unstranded),
        Block("1", 8500, 8600, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_500_1000_2500_2600_6000_6500_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Unstranded),
        Block("1", 2500, 2600, Unstranded),
        Block("1", 6000, 6500, Unstranded),
        Block("1", 8000, 9000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_2500_3000_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 3000, Unstranded),
        Block("1", 8000, 9000, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_4500_5000_6500_6600_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 4500, 5000, Unstranded),
        Block("1", 6500, 6600, Unstranded)
      )), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_7000_8000_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus) === BlockSet(List(
        Block("1", 7000, 8000, Plus),
        Block("1", 9000, 10000, Plus)
      )), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_6000_8000_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus) === BlockSet(List(
        Block("1", 6000, 8000, Plus),
        Block("1", 9000, 10000, Plus)
      )), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus) === Empty, "Self")
      assert(chr1_1500_1600_3500_3600_5500_6500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) ===
        Block("1", 6000, 6500, Plus), "Three blocks nested in three blocks")
      assert(chr1_500_600_1500_1600_3500_4500_5500_6500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
          Block("1", 500, 600, Plus),
          Block("1", 4000, 4500, Plus),
          Block("1", 6000, 6500, Plus)
        )), "Three blocks nested in three blocks")
      assert(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) ===
        Block("1", 8500, 9500, Plus), "Three blocks nested in three blocks")
      assert(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 4500, Plus),
        Block("1", 9000, 10000, Plus)
      )), "Three blocks nested in three blocks")
      assert(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 6000, 6500, Plus)
      )), "Three blocks nested in three blocks")
      assert(chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 8500, 9500, Plus)
      )), "Three blocks nested in three blocks")
      assert(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 4500, Plus),
        Block("1", 8000, 10000, Plus)
      )), "Three blocks nested in three blocks")
      assert(chr1_1100_1200_1300_1400_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === Empty, "Three blocks nested in two blocks")
      assert(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 6500, 6600, Plus),
        Block("1", 8500, 8600, Plus)
      )), "Three blocks nested in two blocks")
      assert(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 6500, Plus)
      )), "Three blocks nested in two blocks")
      assert(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) ===
        Block("1", 6000, 7000, Plus), "Three blocks nested in two blocks")
      assert(chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 6500, 6600, Plus),
        Block("1", 8500, 8600, Plus)
      )), "Three blocks nested in two blocks")
      assert(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 6500, Plus)
      )), "Three blocks nested in two blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2500, 3000, Plus),
        Block("1", 4500, 5000, Plus)
      )), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2500, 3000, Plus),
        Block("1", 4500, 5000, Plus),
        Block("1", 6500, 6600, Plus)
      )), "Three blocks overlapping three blocks")
      assert(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 900, 1000, Plus),
        Block("1", 2500, 2600, Plus),
        Block("1", 4000, 4500, Plus),
        Block("1", 6500, 6600, Plus),
        Block("1", 8000, 8500, Plus)
      )), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2000_3500_4500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4500, 5000, Plus)
      )), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_6000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2500, 3000, Plus),
        Block("1", 4500, 5000, Plus),
        Block("1", 6000, 7000, Plus)
      )), "Three blocks overlapping three blocks")
      assert(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 900, 1000, Plus),
        Block("1", 2500, 2600, Plus),
        Block("1", 6500, 6600, Plus),
        Block("1", 8000, 8500, Plus)
      )), "Three blocks overlapping three blocks")
      assert(chr1_900_1100_1900_2100_2900_4100_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 900, 1000, Plus),
        Block("1", 2000, 2100, Plus),
        Block("1", 2900, 3000, Plus),
        Block("1", 4000, 4100, Plus)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1500_3500_3600_3700_3800_3900_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) ===
        Block("1", 2000, 3000, Plus), "Three blocks overlapping two blocks")
      assert(chr1_3500_4500_5000_5100_5900_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) ===
        Block("1", 4000, 4500, Plus), "Three blocks overlapping two blocks")
      assert(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 2500, Plus),
        Block("1", 9000, 10000, Plus)
      )), "Three blocks overlapping two blocks")
      assert(chr1_900_1100_1900_2100_2900_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 900, 1000, Plus),
        Block("1", 2000, 2100, Plus),
        Block("1", 2900, 3000, Plus),
        Block("1", 4000, 5000, Plus)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1500_3500_3600_3700_3800_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus)
      )), "Three blocks overlapping two blocks")
      assert(chr1_2000_4500_5000_5100_5900_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 4500, Plus)
      )), "Three blocks overlapping two blocks")
      assert(chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2000, 2500, Plus),
        Block("1", 6000, 7000, Plus),
        Block("1", 9000, 10000, Plus)
      )), "Three blocks overlapping two blocks")
      assert(chr1_3000_3100_3200_3300_4500_4600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 4500, 4600, Plus), "Two blocks nested in one block")
      assert(chr1_500_4500_5100_5200_5300_5400_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 4500, Plus)
      )), "Two blocks nested in one block")
      assert(chr1_3000_3100_3200_3300_4500_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) ===
        Block("1", 4500, 5000, Plus), "Two blocks nested in one block")
      assert(chr1_500_5000_5100_5200_5300_5400_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus)
      )), "Two blocks nested in one block")
      assert(chr1_500_1500_3500_4500_plus.minus(chr1_1000_2000_3000_4000_plus) === BlockSet(List(
        Block("1", 500, 1000, Plus),
        Block("1", 4000, 4500, Plus)
      )), "Two blocks overlapping two blocks")
      assert(chr1_1500_2400_2600_3500_plus.minus(chr1_1000_2000_3000_4000_plus) === BlockSet(List(
        Block("1", 2000, 2400, Plus),
        Block("1", 2600, 3000, Plus)
      )), "Two blocks overlapping two blocks")
      assert(chr1_1500_2500_3500_4500_plus.minus(chr1_1000_2000_3000_4000_plus) === BlockSet(List(
        Block("1", 2000, 2500, Plus),
        Block("1", 4000, 4500, Plus)
      )), "Two blocks overlapping two blocks")
      assert(chr1_500_600_3000_4000_6500_6600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 500, 600, Plus),
        Block("1", 6500, 6600, Plus)
      )), "Two blocks same")
      assert(chr1_1000_2000_2500_6500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 6500, Plus)
      )), "Two blocks same")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both) === BlockSet(List(
        Block("1", 2500, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 6500, Plus)
      )), "Two blocks same")
      assert(chr1_900_1100_1900_2100_2900_6000_plus.minus(chr2_900_1100_1900_2100_2900_6000_plus) === BlockSet(List(
        Block("1", 900, 1100, Plus),
        Block("1", 1900, 2100, Plus),
        Block("1", 2900, 6000, Plus)
      )), "Different chromosomes")

    }
  }



}
