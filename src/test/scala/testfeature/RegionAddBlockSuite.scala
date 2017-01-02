package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class RegionAddBlockSuite extends FunSuite {

  addBlockEmpty()
  addBlockBlock()
  addBlockBlockSet()

  def addBlockEmpty(): Unit = {
    test("Empty add block") {
      assert(Empty.addBlock(chr1_1000_2000_unstranded) === Block("1", 1000, 2000, Unstranded),
        "Empty add block should be block renamed")
    }
  }

  def addBlockBlock(): Unit = {
    test("Block add block") {
      // Same span
      assert(chr1_1000_2000_both.addBlock(chr1_1000_2000_minus) === chr1_1000_2000_minus,
        "Block add block with same span should be renamed block with consensus orientation")
      assert(chr1_1000_2000_minus.addBlock(chr1_1000_2000_both) === chr1_1000_2000_minus,
        "Block add block with same span should be renamed block with consensus orientation")
      // Overlapping blocks
      assert(chr1_1000_2000_both.addBlock(chr1_1500_2500_plus) === Block("1", 1000, 2500, Plus),
        "Block add overlapping block should be union block with consensus orientation")
      assert(chr1_1500_2500_plus.addBlock(chr1_1000_2000_both) === Block("1", 1000, 2500, Plus),
        "Block add overlapping block should be union block with consensus orientation")
      // Non-overlapping blocks
      assert(chr2_1000_2000_plus.addBlock(chr2_3000_4000_plus) === BlockSet(List(
        Block("2", 1000, 2000, Plus),
        Block("2", 3000, 4000, Plus))), "Block add non-overlapping block should be block set with consensus orientation")
      assert(chr2_3000_4000_plus.addBlock(chr2_1000_2000_plus) === BlockSet(List(
        Block("2", 1000, 2000, Plus),
        Block("2", 3000, 4000, Plus))), "Block add non-overlapping block should be block set with consensus orientation")
      // Adjacent blocks
      assert(chr1_1000_2000_both.addBlock(chr1_2000_3000_plus) === Block("1", 1000, 3000, Plus),
        "Block add adjacent block should make one big block")
      assert(chr1_2000_3000_plus.addBlock(chr1_1000_2000_both) === Block("1", 1000, 3000, Plus),
        "Block add adjacent block should make one big block")
      // Illegal combinations
      intercept[IllegalArgumentException](chr1_1000_2000_minus.addBlock(chr1_1000_2000_plus_1))
      intercept[IllegalArgumentException](chr1_1000_2000_plus_1.addBlock(chr2_3000_4000_plus))
    }
  }

  def addBlockBlockSet(): Unit = {
    test("BlockSet add block") {
      // Strictly nested both directions
      assert(chr1_900_2100_3000_4000_plus.addBlock(chr1_1000_2000_both) === BlockSet(List(
        Block("1", 900, 2100, Plus),
        Block("1", 3000, 4000, Plus)
      )), "One block of block set completely contains the other block")
      assert(chr1_1100_1200_1300_1400_plus.addBlock(chr1_1000_2000_both) ===
        Block("1", 1000, 2000, Plus), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_1100_1200_1300_plus.addBlock(chr1_1000_2000_both) ===
        Block("1", 1000, 2000, Plus), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_1100_1900_2000_plus.addBlock(chr1_1000_2000_both) ===
        Block("1", 1000, 2000, Plus), "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(chr1_1000_2000_both) ===
        Block("1", 1000, 2000, Plus), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("1", 1000, 1050, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1900, 2000, Plus)
      )), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("1", 1050, 1100, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1900, 2000, Plus)
      )), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("1", 1200, 1250, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1900, 2000, Plus)
      )), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("1", 1250, 1300, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1900, 2000, Plus)
      )), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("1", 1900, 1950, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1900, 2000, Plus)
      )), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("1", 1950, 2000, Plus)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1200, 1300, Plus),
        Block("1", 1900, 2000, Plus)
      )), "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1900_2100_3000_4000_plus.addBlock(chr1_1000_2000_both) === BlockSet(List(
        Block("1", 1000, 2100, Plus),
        Block("1", 3000, 4000, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1900_2100_3000_4000_plus.addBlock(chr1_1500_2500_plus) === BlockSet(List(
        Block("1", 1500, 2500, Plus),
        Block("1", 3000, 4000, Plus)
      )), "Overlapping one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1400, 1550, Plus)) === BlockSet(List(
        Block("1", 1400, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1550, 1650, Plus)) === BlockSet(List(
        Block("1", 1500, 1650, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1650, 1750, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1650, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1750, 1850, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1850, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 2000, 2150, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2000, 2300, Plus)
      )), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 2200, 2350, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2350, Plus)
      )), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1700_1800_1900_2100_plus.addBlock(chr1_1000_2000_both) === Block("1", 1000, 2100, Plus), "Overlapping two blocks off end")
      assert(chr1_1700_1800_1900_2100_plus.addBlock(Block("1", 1750, 2200, Plus)) ===
        Block("1", 1700, 2200, Plus), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1400, 1750, Plus)) === BlockSet(List(
        Block("1", 1400, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1550, 1850, Plus)) === BlockSet(List(
        Block("1", 1500, 1850, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1650, 2200, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1650, 2300, Plus)
      )), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1750, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 2400, Plus)
      )), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1500_1600_2100_2300_plus.addBlock(chr1_1000_2000_both) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping one block and an intron")
      assert(chr1_1500_1600_2100_2300_plus.addBlock(chr1_2000_3000_plus) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 2000, 3000, Plus)
      )), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(chr1_1000_2000_both) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Overlapping two blocks and an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1650, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1650, 2400, Plus)
      )), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.addBlock(Block("1", 500, 6500, Plus)) === BlockSet(List(
        Block("1", 500, 6500, Plus),
        Block("1", 7000, 8000, Plus)
      )), "Overlapping three blocks and an intron")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.addBlock(Block("1", 2500, 8500, Plus)) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 2500, 8500, Plus)
      )), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_3000_4000_plus.addBlock(chr1_1000_2000_both) === BlockSet(List(
        Block("1", 1000, 2000, Plus),
        Block("1", 3000, 4000, Plus)
      )), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1500, 1600, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1700, 1800, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 2100, 2300, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_900_1100_1500_1600_1900_2100_plus.addBlock(chr1_1000_2000_both) ===
        Block("1", 900, 2100, Plus), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.addBlock(Block("1", 1500, 7500, Plus)) ===
        Block("1", 1000, 8000, Plus), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1400, 1600, Plus)) === BlockSet(List(
        Block("1", 1400, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1500, 1650, Plus)) === BlockSet(List(
        Block("1", 1500, 1650, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1650, 1800, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1650, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1700, 1850, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1850, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 2000, 2300, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2000, 2300, Plus)
      )), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 2100, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2400, Plus)
      )), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_100_200_300_400_plus.addBlock(chr1_1000_2000_plus_1) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus),
        Block("1", 1000, 2000, Plus)
      )), "Non-overlapping because different spans")
      assert(chr1_100_200_300_400_plus.addBlock(Block("1", 500, 600, Both)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus),
        Block("1", 500, 600, Plus)
      )), "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1000, 1100, Both)) === BlockSet(List(
        Block("1", 1000, 1100, Plus),
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 2400, 2500, Both)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus),
        Block("1", 2400, 2500, Plus)
      )), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      intercept[IllegalArgumentException](chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(chr1_1000_2000_minus))
      // Different chromosomes
      intercept[IllegalArgumentException](chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.addBlock(chr2_1000_2000_plus))
      // Same span as an intron
      // Two blocks
      assert(chr1_100_200_300_400_plus.addBlock(Block("1", 200, 300, Plus)) === Block("1", 100, 400, Plus), "Same span as an intron")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1600, 1700, Plus)) === BlockSet(List(
        Block("1", 1500, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1800, 2100, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 2300, Plus)
      )), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(chr1_100_200_300_400_plus.addBlock(Block("1", 50, 100, Plus)) === BlockSet(List(
        Block("1", 50, 200, Plus),
        Block("1", 300, 400, Plus)
      )), "Adjacent to span")
      assert(chr1_100_200_300_400_plus.addBlock(Block("1", 400, 500, Plus)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 500, Plus)
      )), "Adjacent to span")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1400, 1500, Plus)) === BlockSet(List(
        Block("1", 1400, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 2300, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2400, Plus)
      )), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(chr1_100_200_300_400_plus.addBlock(Block("1", 201, 300, Plus)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 201, 400, Plus)
      )), "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.addBlock(Block("1", 200, 299, Plus)) === BlockSet(List(
        Block("1", 100, 299, Plus),
        Block("1", 300, 400, Plus)
      )), "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.addBlock(Block("1", 201, 299, Plus)) === BlockSet(List(
        Block("1", 100, 200, Plus),
        Block("1", 201, 299, Plus),
        Block("1", 300, 400, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1801, 2099, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 1801, 2099, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1601, 1699, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1601, 1699, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1801, 2099, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 1801, 2099, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1600, 1699, Plus)) === BlockSet(List(
        Block("1", 1500, 1699, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1800, 2099, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 2099, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1601, 1700, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1601, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      // Outside span
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1400, 1499, Plus)) === BlockSet(List(
        Block("1", 1400, 1499, Plus),
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 2301, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus),
        Block("1", 2301, 2400, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 1400, 1500, Plus)) === BlockSet(List(
        Block("1", 1400, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2300, Plus)
      )), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("1", 2300, 2400, Plus)) === BlockSet(List(
        Block("1", 1500, 1600, Plus),
        Block("1", 1700, 1800, Plus),
        Block("1", 2100, 2400, Plus)
      )), "1bp gap between blocks")
    }
  }


}
