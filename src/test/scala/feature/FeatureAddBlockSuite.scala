package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class FeatureAddBlockSuite extends FunSuite {

  addBlockEmpty()
  addBlockBlock()
  addBlockBlockSet()

  def addBlockEmpty(): Unit = {
    test("Empty add block") {
      assert(Empty.addBlock(chr1_1000_2000_unstranded, "name") === Block("chr1", 1000, 2000, Unstranded, "name"),
        "Empty add block should be block renamed")
    }
  }

  def addBlockBlock(): Unit = {
    test("Block add block") {
      // Same span
      assert(chr1_1000_2000_both.addBlock(chr1_1000_2000_minus, "name") === chr1_1000_2000_minus.rename("name"),
        "Block add block with same span should be renamed block with consensus orientation")
      assert(chr1_1000_2000_minus.addBlock(chr1_1000_2000_both, "name") === chr1_1000_2000_minus.rename("name"),
        "Block add block with same span should be renamed block with consensus orientation")
      // Overlapping blocks
      assert(chr1_1000_2000_both.addBlock(chr1_1500_2500_plus, "name") === Block("chr1", 1000, 2500, Plus, "name"),
        "Block add overlapping block should be union block with consensus orientation")
      assert(chr1_1500_2500_plus.addBlock(chr1_1000_2000_both, "name") === Block("chr1", 1000, 2500, Plus, "name"),
        "Block add overlapping block should be union block with consensus orientation")
      // Non-overlapping blocks
      assert(chr2_1000_2000_plus.addBlock(chr2_3000_4000_plus, "name") === BlockSet(List(
        Block("chr2", 1000, 2000, Plus, ""),
        Block("chr2", 3000, 4000, Plus, "")), "name"), "Block add non-overlapping block should be block set with consensus orientation")
      assert(chr2_3000_4000_plus.addBlock(chr2_1000_2000_plus, "name") === BlockSet(List(
        Block("chr2", 1000, 2000, Plus, ""),
        Block("chr2", 3000, 4000, Plus, "")), "name"), "Block add non-overlapping block should be block set with consensus orientation")
      // Adjacent blocks
      assert(chr1_1000_2000_both.addBlock(chr1_2000_3000_plus, "name") === Block("chr1", 1000, 3000, Plus, "name"),
        "Block add adjacent block should make one big block")
      assert(chr1_2000_3000_plus.addBlock(chr1_1000_2000_both, "name") === Block("chr1", 1000, 3000, Plus, "name"),
        "Block add adjacent block should make one big block")
      // Illegal combinations
      intercept[IllegalArgumentException](chr1_1000_2000_minus.addBlock(chr1_1000_2000_plus_1, "name"))
      intercept[IllegalArgumentException](chr1_1000_2000_plus_1.addBlock(chr2_3000_4000_plus, "name"))
    }
  }

  def addBlockBlockSet(): Unit = {
    test("BlockSet add block") {
      // Strictly nested both directions
      assert(chr1_900_2100_3000_4000_plus.addBlock(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 900, 2100, Plus, ""),
        Block("chr1", 3000, 4000, Plus, "")
      ), "name"), "One block of block set completely contains the other block")
      assert(chr1_1100_1200_1300_1400_plus.addBlock(chr1_1000_2000_both, "name") ===
        Block("chr1", 1000, 2000, Plus, "name"), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_1100_1200_1300_plus.addBlock(chr1_1000_2000_both, "name") ===
        Block("chr1", 1000, 2000, Plus, "name"), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_1100_1900_2000_plus.addBlock(chr1_1000_2000_both, "name") ===
        Block("chr1", 1000, 2000, Plus, "name"), "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(chr1_1000_2000_both, "name") ===
        Block("chr1", 1000, 2000, Plus, "name"), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("chr1", 1000, 1050, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("chr1", 1050, 1100, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("chr1", 1200, 1250, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("chr1", 1250, 1300, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("chr1", 1900, 1950, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.addBlock(Block("chr1", 1950, 2000, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1900_2100_3000_4000_plus.addBlock(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1000, 2100, Plus, ""),
        Block("chr1", 3000, 4000, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1900_2100_3000_4000_plus.addBlock(chr1_1500_2500_plus, "name") === BlockSet(List(
        Block("chr1", 1500, 2500, Plus, ""),
        Block("chr1", 3000, 4000, Plus, "")
      ), "name"), "Overlapping one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1400, 1550, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1400, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1550, 1650, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1650, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1650, 1750, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1650, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1750, 1850, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1850, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 2000, 2150, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2000, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 2200, 2350, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2350, Plus, "")
      ), "name"), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1700_1800_1900_2100_plus.addBlock(chr1_1000_2000_both, "name") === Block("chr1", 1000, 2100, Plus, "name"), "Overlapping two blocks off end")
      assert(chr1_1700_1800_1900_2100_plus.addBlock(Block("chr1", 1750, 2200, Plus, "block"), "name") ===
        Block("chr1", 1700, 2200, Plus, "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1400, 1750, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1400, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1550, 1850, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1850, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1650, 2200, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1650, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1750, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 2400, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1500_1600_2100_2300_plus.addBlock(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1000, 2000, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping one block and an intron")
      assert(chr1_1500_1600_2100_2300_plus.addBlock(chr1_2000_3000_plus, "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 2000, 3000, Plus, "")
      ), "name"), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1000, 2000, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks and an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1650, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1650, 2400, Plus, "")
      ), "name"), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.addBlock(Block("chr1", 500, 6500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 500, 6500, Plus, ""),
        Block("chr1", 7000, 8000, Plus, "")
      ), "name"), "Overlapping three blocks and an intron")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.addBlock(Block("chr1", 2500, 8500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 2000, Plus, ""),
        Block("chr1", 2500, 8500, Plus, "")
      ), "name"), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_3000_4000_plus.addBlock(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1000, 2000, Plus, ""),
        Block("chr1", 3000, 4000, Plus, "")
      ), "name"), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1500, 1600, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1700, 1800, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 2100, 2300, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_900_1100_1500_1600_1900_2100_plus.addBlock(chr1_1000_2000_both, "name") ===
        Block("chr1", 900, 2100, Plus, "name"), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.addBlock(Block("chr1", 1500, 7500, Plus, "block"), "name") ===
        Block("chr1", 1000, 8000, Plus, "name"), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1400, 1600, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1400, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1500, 1650, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1650, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1650, 1800, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1650, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1700, 1850, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1850, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 2000, 2300, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2000, 2300, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 2100, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2400, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_100_200_300_400_plus.addBlock(chr1_1000_2000_plus_1, "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, ""),
        Block("chr1", 1000, 2000, Plus, "")
      ), "name"), "Non-overlapping because different spans")
      assert(chr1_100_200_300_400_plus.addBlock(Block("chr1", 500, 600, Both, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, ""),
        Block("chr1", 500, 600, Plus, "")
      ), "name"), "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1000, 1100, Both, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 2400, 2500, Both, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, ""),
        Block("chr1", 2400, 2500, Plus, "")
      ), "name"), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      intercept[IllegalArgumentException](chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(chr1_1000_2000_minus, "name"))
      // Different chromosomes
      intercept[IllegalArgumentException](chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.addBlock(chr2_1000_2000_plus, "name"))
      // Same span as an intron
      // Two blocks
      assert(chr1_100_200_300_400_plus.addBlock(Block("chr1", 200, 300, Plus, "block"), "name") === Block("chr1", 100, 400, Plus, "name"), "Same span as an intron")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1600, 1700, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1800, 2100, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 2300, Plus, "")
      ), "name"), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(chr1_100_200_300_400_plus.addBlock(Block("chr1", 50, 100, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 50, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "Adjacent to span")
      assert(chr1_100_200_300_400_plus.addBlock(Block("chr1", 400, 500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 500, Plus, "")
      ), "name"), "Adjacent to span")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1400, 1500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1400, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 2300, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2400, Plus, "")
      ), "name"), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(chr1_100_200_300_400_plus.addBlock(Block("chr1", 201, 300, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 201, 400, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.addBlock(Block("chr1", 200, 299, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 299, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.addBlock(Block("chr1", 201, 299, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 201, 299, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1801, 2099, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 1801, 2099, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1601, 1699, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1601, 1699, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1801, 2099, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 1801, 2099, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1600, 1699, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1699, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1800, 2099, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 2099, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1601, 1700, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1601, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      // Outside span
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1400, 1499, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1400, 1499, Plus, ""),
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 2301, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, ""),
        Block("chr1", 2301, 2400, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 1400, 1500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1400, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.addBlock(Block("chr1", 2300, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2400, Plus, "")
      ), "name"), "1bp gap between blocks")
    }
  }


}
