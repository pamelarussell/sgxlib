package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class FeatureMinusSuite extends FunSuite {

  minusEmpty()
  minusBlockEmpty()
  minusBlockBlock()
  minusBlockBlockSet()
  minusBlockSetEmpty()
  minusBlockSetBlock()
  minusBlockSetBlockSet()

  def minusEmpty(): Unit = {
    test("Empty minus") {
      assert(Empty.minus(Empty, "name") === Empty, "Empty minus empty should be empty")
      assert(Empty.minus(chr1_100_200_300_400_plus, "name") == Empty, "Empty minus block set should be empty")
      assert(Empty.minus(chr1_1000_2000_unstranded, "name") === Empty, "Empty minus block should be empty")
    }
  }

  def minusBlockEmpty(): Unit = {
    test("Block minus Empty") {
      assert(chr1_1000_2000_unstranded.minus(Empty, "name") === chr1_1000_2000_unstranded.rename("name"),
        "Block minus empty should be original block")
    }
  }

  def minusBlockBlock(): Unit = {
    test("Block minus Block") {
      // Nested blocks
      assert(chr1_1000_2000_both.minus(chr1_1200_1300_plus, "name") === BlockSet(
        List(Block("chr1", 1000, 1200, Both, ""), Block("chr1", 1300, 2000, Both, ""))
        , "name"))
      assert(chr1_1200_1300_plus.minus(chr1_1000_2000_both, "name") === Empty,
        "Block minus containing block should be empty")
      // Nested blocks sharing an endpoint
      assert(chr1_4_6_minus.minus(chr1_5_6_minus, "name") === Block("chr1", 4, 5, Minus, "name"),
        "Block minus nested block sharing one endpoint should be the rest of the interval")
      // Overlapping blocks
      assert(chr1_1000_2000_both.minus(chr1_1500_2500_plus, "name") === Block("chr1", 1000, 1500, Both, "name"),
        "Block minus overlapping block should be the rest of the interval")
      // Non-overlapping blocks
      assert(chr2_1000_2000_plus.minus(chr2_3000_4000_plus, "name") === chr2_1000_2000_plus.rename("name"),
        "Block minus non-overlapping block should be original block")
      assert(chr1_1000_2000_minus.minus(chr1_1200_1300_plus, "name") === chr1_1000_2000_minus.rename("name"),
        "Block minus non-overlapping block should be original block")
      assert(chr2_1000_2000_plus.minus(chr1_1000_2000_plus_1, "name") === chr2_1000_2000_plus.rename("name"),
        "Block minus block on other chromosome")
      // Same span
      assert(chr1_1000_2000_minus.minus(chr1_1000_2000_both, "name") === Empty,
        "Block minus block with same span should be empty")
    }
  }

  def minusBlockBlockSet(): Unit = {
    test("Block minus BlockSet") {
      // Strictly nested both directions
      assert(chr1_1000_2000_both.minus(chr1_900_2100_3000_4000_plus, "name") === Empty, "One block of block set completely contains the other block")
      assert(chr1_1000_2000_both.minus(chr1_1100_1200_1300_1400_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Both, ""),
        Block("chr1", 1200, 1300, Both, ""),
        Block("chr1", 1400, 2000, Both, "")
      ), "name"), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_2000_both.minus(chr1_1000_1100_1200_1300_plus, "name") === BlockSet(List(
        Block("chr1", 1100, 1200, Both, ""),
        Block("chr1", 1300, 2000, Both, "")
      ), "name"), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_2000_both.minus(chr1_1000_1100_1900_2000_plus, "name") === Block("chr1", 1100, 1900, Both, "name"),
        "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_2000_both.minus(chr1_1000_1100_1200_1300_1900_2000_plus, "name") === BlockSet(List(
        Block("chr1", 1100, 1200, Both, ""),
        Block("chr1", 1300, 1900, Both, "")
      ), "name"), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(Block("chr1", 1000, 1050, Plus, "block").minus(chr1_1000_1100_1200_1300_1900_2000_plus, "name") === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1050, 1100, Plus, "block").minus(chr1_1000_1100_1200_1300_1900_2000_plus, "name") === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1200, 1250, Plus, "block").minus(chr1_1000_1100_1200_1300_1900_2000_plus, "name") === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1250, 1300, Plus, "block").minus(chr1_1000_1100_1200_1300_1900_2000_plus, "name") === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1900, 1950, Plus, "block").minus(chr1_1000_1100_1200_1300_1900_2000_plus, "name") === Empty,
        "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1950, 2000, Plus, "block").minus(chr1_1000_1100_1200_1300_1900_2000_plus, "name") === Empty,
        "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1000_2000_both.minus(chr1_1900_2100_3000_4000_plus, "name") === Block("chr1", 1000, 1900, Both, "name"),
        "Overlapping one block off end")
      assert(chr1_1500_2500_plus.minus(chr1_1900_2100_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", 1500, 1900, Plus, ""),
        Block("chr1", 2100, 2500, Plus, "")
      ), "name"),
        "Overlapping one block off end")
      assert(Block("chr1", 1400, 1550, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1400, 1500, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 1550, 1650, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1600, 1650, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 1650, 1750, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1650, 1700, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 1750, 1850, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1800, 1850, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 2000, 2150, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2000, 2100, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 2200, 2350, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2300, 2350, Plus, "name"), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1000_2000_both.minus(chr1_1700_1800_1900_2100_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 1700, Both, ""),
        Block("chr1", 1800, 1900, Both, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1750, 2200, Plus, "block").minus(chr1_1700_1800_1900_2100_plus, "name") === BlockSet(List(
        Block("chr1", 1800, 1900, Plus, ""),
        Block("chr1", 2100, 2200, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1400, 1750, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1400, 1500, Plus, ""),
        Block("chr1", 1600, 1700, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1550, 1850, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1600, 1700, Plus, ""),
        Block("chr1", 1800, 1850, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1650, 2200, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1650, 1700, Plus, ""),
        Block("chr1", 1800, 2100, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1750, 2400, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1800, 2100, Plus, ""),
        Block("chr1", 2300, 2400, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1000_2000_both.minus(chr1_1500_1600_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 1500, Both, ""),
        Block("chr1", 1600, 2000, Both, "")
      ), "name"), "Overlapping one block and an intron")
      assert(chr1_2000_3000_plus.minus(chr1_1500_1600_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 2000, 2100, Plus, ""),
        Block("chr1", 2300, 3000, Plus, "")
      ), "name"), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1000_2000_both.minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 1500, Both, ""),
        Block("chr1", 1600, 1700, Both, ""),
        Block("chr1", 1800, 2000, Both, "")
      ), "name"), "Overlapping two blocks and an intron")
      assert(Block("chr1", 1650, 2400, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1650, 1700, Plus, ""),
        Block("chr1", 1800, 2100, Plus, ""),
        Block("chr1", 2300, 2400, Plus, "")
      ), "name"), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(Block("chr1", 500, 6500, Plus, "block").minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", 500, 1000, Plus, ""),
        Block("chr1", 2000, 3000, Plus, ""),
        Block("chr1", 4000, 5000, Plus, ""),
        Block("chr1", 6000, 6500, Plus, "")
      ), "name"), "Overlapping three blocks and an intron")
      assert(Block("chr1", 2500, 8500, Plus, "block").minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", 2500, 3000, Plus, ""),
        Block("chr1", 4000, 5000, Plus, ""),
        Block("chr1", 6000, 7000, Plus, ""),
        Block("chr1", 8000, 8500, Plus, "")
      ), "name"), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_both.minus(chr1_1000_2000_3000_4000_plus, "name") === Empty, "Same span as one block")
      assert(Block("chr1", 1500, 1600, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Same span as one block")
      assert(Block("chr1", 1700, 1800, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Same span as one block")
      assert(Block("chr1", 2100, 2300, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_1000_2000_both.minus(chr1_900_1100_1500_1600_1900_2100_plus, "name") === BlockSet(List(
        Block("chr1", 1100, 1500, Both, ""),
        Block("chr1", 1600, 1900, Both, "")
      ), "name"), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(Block("chr1", 1500, 7500, Plus, "block").minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", 2000, 3000, Plus, ""),
        Block("chr1", 4000, 5000, Plus, ""),
        Block("chr1", 6000, 7000, Plus, "")
      ), "name"), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(Block("chr1", 1400, 1600, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1400, 1500, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 1500, 1650, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1600, 1650, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 1650, 1800, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1650, 1700, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 1700, 1850, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1800, 1850, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 2000, 2300, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2000, 2100, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 2100, 2400, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2300, 2400, Plus, "name"), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_1000_2000_plus_1.minus(chr1_100_200_300_400_plus, "name") ===
        chr1_1000_2000_plus_1.rename("name"), "Non-overlapping because different spans")
      assert(Block("chr1", 500, 600, Both, "block").minus(chr1_100_200_300_400_plus, "name") ===
        Block("chr1", 500, 600, Both, "name"), "Non-overlapping because different spans")
      assert(Block("chr1", 1000, 1100, Both, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1000, 1100, Both, "name"), "Non-overlapping because different spans")
      assert(Block("chr1", 2400, 2500, Both, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2400, 2500, Both, "name"), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(chr1_1000_2000_minus.minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        chr1_1000_2000_minus.rename("name"), "Non-overlapping because different orientations")
      // Different chromosomes
      assert(chr2_1000_2000_plus.minus(chr1_2000_3000_plus, "name") ===
        chr2_1000_2000_plus.rename("name"), "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(Block("chr1", 200, 300, Plus, "block").minus(chr1_100_200_300_400_plus, "name") ===
        Block("chr1", 200, 300, Plus, "name"), "Same span as an intron")
      // Three blocks
      assert(Block("chr1", 1600, 1700, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1600, 1700, Plus, "name"), "Same span as an intron")
      assert(Block("chr1", 1800, 2100, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1800, 2100, Plus, "name"), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(Block("chr1", 50, 100, Plus, "block").minus(chr1_100_200_300_400_plus, "name") ===
        Block("chr1", 50, 100, Plus, "name"), "Adjacent to span")
      assert(Block("chr1", 400, 500, Plus, "block").minus(chr1_100_200_300_400_plus, "name") ===
        Block("chr1", 400, 500, Plus, "name"), "Adjacent to span")
      // Three blocks
      assert(Block("chr1", 1400, 1500, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1400, 1500, Plus, "name"), "Same span as an intron")
      assert(Block("chr1", 2300, 2400, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2300, 2400, Plus, "name"), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(Block("chr1", 201, 300, Plus, "block").minus(chr1_100_200_300_400_plus, "name") ===
        Block("chr1", 201, 300, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 200, 299, Plus, "block").minus(chr1_100_200_300_400_plus, "name") ===
        Block("chr1", 200, 299, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 201, 299, Plus, "block").minus(chr1_100_200_300_400_plus, "name") ===
        Block("chr1", 201, 299, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 1801, 2099, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1801, 2099, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 1601, 1699, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1601, 1699, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 1801, 2099, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1801, 2099, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 1600, 1699, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1600, 1699, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 1800, 2099, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1800, 2099, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 1601, 1700, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1601, 1700, Plus, "name"), "1bp gap between blocks")
      // Outside span
      assert(Block("chr1", 1400, 1499, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1400, 1499, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 2301, 2400, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2301, 2400, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 1400, 1500, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1400, 1500, Plus, "name"), "1bp gap between blocks")
      assert(Block("chr1", 2300, 2400, Plus, "block").minus(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2300, 2400, Plus, "name"), "1bp gap between blocks")
    }
  }

  def minusBlockSetEmpty(): Unit = {
    test("BlockSet minus Empty") {
      assert(chr1_100_200_300_400_plus.minus(Empty, "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "Block set minus empty")
    }
  }

  def minusBlockSetBlock(): Unit = {
    test("BlockSet minus Block") {
      // Strictly nested both directions
      assert(chr1_900_2100_3000_4000_plus.minus(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 900, 1000, Plus, ""),
        Block("chr1", 2000, 2100, Plus, ""),
        Block("chr1", 3000, 4000, Plus, "")
      ), "name"), "One block of block set completely contains the other block")
      assert(chr1_1100_1200_1300_1400_plus.minus(chr1_1000_2000_both, "name") === Empty, "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_1100_1200_1300_plus.minus(chr1_1000_2000_both, "name") === Empty, "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_1100_1900_2000_plus.minus(chr1_1000_2000_both, "name") === Empty, "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(chr1_1000_2000_both, "name") === Empty,
        "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("chr1", 1000, 1050, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1050, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("chr1", 1050, 1100, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1050, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("chr1", 1200, 1250, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1250, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("chr1", 1250, 1300, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1250, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("chr1", 1900, 1950, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1950, 2000, Plus, "")
      ), "name"),
        "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.minus(Block("chr1", 1950, 2000, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 1950, Plus, "")
      ), "name"),
        "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1900_2100_3000_4000_plus.minus(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 2000, 2100, Plus, ""),
        Block("chr1", 3000, 4000, Plus, "")
      ), "name"),
        "Overlapping one block off end")
      assert(chr1_1900_2100_3000_4000_plus.minus(chr1_1500_2500_plus, "name") === Block("chr1", 3000, 4000, Plus, "name"),
        "Overlapping one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1400, 1550, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1550, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1550, 1650, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1550, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1650, 1750, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1750, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1750, 1850, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1750, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 2000, 2150, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2150, 2300, Plus, "")
      ), "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 2200, 2350, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2200, Plus, "")
      ), "name"), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1700_1800_1900_2100_plus.minus(chr1_1000_2000_both, "name") === Block("chr1", 2000, 2100, Plus, "name"),
        "Overlapping two blocks off end")
      assert(chr1_1700_1800_1900_2100_plus.minus(Block("chr1", 1750, 2200, Plus, "block"), "name") === Block("chr1", 1700, 1750, Plus, "name"),
        "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1400, 1750, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1750, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1550, 1850, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1550, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1650, 2200, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 2200, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1750, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1750, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1500_1600_2100_2300_plus.minus(chr1_1000_2000_both, "name") === Block("chr1", 2100, 2300, Plus, "name"),
        "Overlapping one block and an intron")
      assert(chr1_1500_1600_2100_2300_plus.minus(chr1_2000_3000_plus, "name") === Block("chr1", 1500, 1600, Plus, "name"),
        "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(chr1_1000_2000_both, "name") === Block("chr1", 2100, 2300, Plus, "name"),
        "Overlapping two blocks and an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1650, 2400, Plus, "block"), "name") ===
        Block("chr1", 1500, 1600, Plus, "name"), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.minus(Block("chr1", 500, 6500, Plus, "block"), "name") ===
        Block("chr1", 7000, 8000, Plus, "name"), "Overlapping three blocks and an intron")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.minus(Block("chr1", 2500, 8500, Plus, "block"), "name") ===
        Block("chr1", 1000, 2000, Plus, "name"), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1000_2000_both, "name") ===
        Block("chr1", 3000, 4000, Plus, "name"), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1500, 1600, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1700, 1800, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 2100, 2300, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, "")
      ), "name"), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_900_1100_1500_1600_1900_2100_plus.minus(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 900, 1000, Plus, ""),
        Block("chr1", 2000, 2100, Plus, "")
      ), "name"), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.minus(Block("chr1", 1500, 7500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 1500, Plus, ""),
        Block("chr1", 7500, 8000, Plus, "")
      ), "name"), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1400, 1600, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1500, 1650, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1650, 1800, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1700, 1850, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 2000, 2300, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 2100, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, "")
      ), "name"), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_100_200_300_400_plus.minus(chr1_1000_2000_plus_1, "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "Non-overlapping because different spans")
      assert(chr1_100_200_300_400_plus.minus(Block("chr1", 500, 600, Both, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1000, 1100, Both, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 2400, 2500, Both, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(chr1_1000_2000_minus, "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Non-overlapping because different orientations")
      // Different chromosomes
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.minus(chr2_1000_2000_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 2000, Minus, ""),
        Block("chr1", 3000, 4000, Minus, ""),
        Block("chr1", 5000, 6000, Minus, ""),
        Block("chr1", 7000, 8000, Minus, "")
      ), "name"), "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(chr1_100_200_300_400_plus.minus(Block("chr1", 200, 300, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "Same span as an intron")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1600, 1700, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1800, 2100, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(chr1_100_200_300_400_plus.minus(Block("chr1", 50, 100, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "Adjacent to span")
      assert(chr1_100_200_300_400_plus.minus(Block("chr1", 400, 500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "Adjacent to span")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1400, 1500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 2300, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(chr1_100_200_300_400_plus.minus(Block("chr1", 201, 300, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.minus(Block("chr1", 200, 299, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.minus(Block("chr1", 201, 299, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, ""),
        Block("chr1", 300, 400, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1801, 2099, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1601, 1699, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1801, 2099, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1600, 1699, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1800, 2099, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1601, 1700, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      // Outside span
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1400, 1499, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 2301, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 1400, 1500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.minus(Block("chr1", 2300, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "1bp gap between blocks")
    }
  }

  def minusBlockSetBlockSet(): Unit = {
    ignore("BlockSet minus BlockSet") {

      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1500_1600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_1500_1600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2500_2600_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2500_2600_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_3500_3600_4500_4600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_3500_3600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_5500_5600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_5500_5600_6500_6600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_1500_1600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_1000_1500_1600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_1000_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_1000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2000_3000_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2500_3000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_3000_3500_3600_4500_4600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_2600_3500_3600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_3000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_4000_4600_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_4000_5000_5500_5600_6000_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_5500_5600_6000_6600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_600_3500_3600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1100_1200_2500_3000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1100_1200_4000_4600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_2000_3000_3500_3600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_900_1100_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_700_800_900_2000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_600_900_1100_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_600_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_600_900_2000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_2500_4000_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_3500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_4000_4500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_2700_3500_6000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_3500_4000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_4000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_2000_5000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_5000_6000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_5000_7000_8000_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_4500_4600_5500_6500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_3000_4000_5000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2100_2200_2300_2400_2500_3100_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2100_2200_2300_3000_4000_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1500_3500_3600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1000_2000_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_4500_5500_5600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1500_2500_2600_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_3500_3600_3800_4500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_3600_4500_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_3000_3500_3600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1000_3000_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_4500_5500_5600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1500_2500_2600_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_2600_3500_3600_3800_4500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_3600_4000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_6500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_5500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_5500_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_3500_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_2600_2800_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_8500_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_5500_6000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_1500_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_1000_1500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_5500_6500_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_2000_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_2600_2800_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_8500_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_4500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_3500_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_5500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_3500_4500_4600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_4500_5500_6500_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_4500_5500_6500_7500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_4500_6500_8500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_5000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_5000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3500_5500_6000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_4000_4500_4600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_5000_5500_6500_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_4500_6000_6500_7500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_4500_6500_8500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_600_1000_2000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_4000_5500_6500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_2600_4500_4600_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_2500_3500_4500_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_2500_3000_4500_4600_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1500_2500_3000_4500_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same blocks compatible orientation")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_7000_8000_9000_10000_minus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, different orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_1000_4000_7000_8000_minus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, different orientation, overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_6000_7000_8000_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, adjacent spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_2000_3000_4000_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_3000_4000_5000_6000_7000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_3000_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_3000_4000_5000_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_900_1000_4000_5000_6000_7000_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_2600_4500_4600_6500_6600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_4500_4600_6500_6600_8500_8600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1000_2500_2600_6000_6500_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2500_3000_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_4500_5000_6500_6600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_7000_8000_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_6000_8000_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Self")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_3500_3600_5500_6500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_1500_1600_3500_4500_5500_6500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1100_1200_1300_1400_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1500_2500_3500_4500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1500_2000_3500_4500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_1500_2500_3500_4500_5500_6000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_900_1100_1900_2100_2900_4100_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_3500_3600_3700_3800_3900_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_3500_4500_5000_5100_5900_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_900_1100_1900_2100_2900_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1500_3500_3600_3700_3800_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_2000_4500_5000_5100_5900_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_3100_3200_3300_4500_4600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_4500_5100_5200_5300_5400_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_3000_3100_3200_3300_4500_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.minus(chr1_500_5000_5100_5200_5300_5400_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_500_1500_3500_4500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1500_2400_2600_3500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_plus.minus(chr1_1500_2500_3500_4500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_500_600_3000_4000_6500_6600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1000_2000_2500_6500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.minus(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks same")
      assert(chr1_500_600_1500_1600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_1500_1600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_2600_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_2600_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_3500_3600_4500_4600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_3500_3600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_5500_5600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_5500_5600_6500_6600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_1000_1500_1600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_1500_1600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_2000_3000_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_3000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_2000_3000_3500_3600_4500_4600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_2000_2600_3500_3600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_2500_3000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_4000_4600_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_4000_5000_5500_5600_6000_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_5500_5600_6000_6600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_3500_3600_both.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1100_1200_2500_3000_both.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1100_1200_4000_4600_both.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_2000_3000_3500_3600_both.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_900_1100_both.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_1000_2000_both.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_900_2000_both.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_500_600_900_1100_both.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_500_600_1000_2000_both.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_500_600_900_2000_both.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1500_2500_4000_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_2500_3500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_3000_4000_4500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_2500_2600_2700_3500_6000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_500_600_3500_4000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_500_600_4000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_500_600_2000_5000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_3500_5000_6000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_5000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_500_1000_5000_7000_8000_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_4500_4600_5500_6500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_2000_3000_4000_5000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_2100_2200_2300_2400_2500_3100_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_2100_2200_2300_3000_4000_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_500_1500_3500_3600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_4500_5500_5600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1500_2500_2600_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_2500_2600_3500_3600_3800_4500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_3600_4500_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_3000_3500_3600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_3000_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_3000_4500_5500_5600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1500_2500_2600_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_2000_2600_3500_3600_3800_4500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_3600_4000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_6500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1500_5500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_500_600_1000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_500_600_1500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_500_5500_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_500_600_3500_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_2500_2600_2800_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_2500_8500_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1500_5500_6000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_500_1000_1500_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_500_1000_1500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_500_5500_6500_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_500_600_2000_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_2000_2600_2800_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_2000_8500_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_500_4500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_3500_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_3500_5500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_3000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_500_3500_4500_4600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_3500_4500_5500_6500_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_4500_5500_6500_7500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_4500_6500_8500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_500_5000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_5000_5500_5600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_3500_5500_6000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_2000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_500_4000_4500_4600_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_3500_5000_5500_6500_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_4500_6000_6500_7500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_2000_4500_6500_8500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_500_600_1000_2000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_3000_4000_5500_6500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_2500_2600_4500_4600_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1500_2500_3500_4500_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_5000_6000_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_2500_3000_4500_4600_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1500_2500_3000_4500_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same blocks compatible orientation")
      assert(chr1_7000_8000_9000_10000_minus.minus(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, different orientation, non-overlapping spans")
      assert(chr1_1000_4000_7000_8000_minus.minus(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, different orientation, overlapping spans")
      assert(chr1_6000_7000_8000_9000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, adjacent spans")
      assert(chr1_2000_3000_4000_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_2000_3000_4000_5000_6000_7000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_2000_3000_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_2000_3000_4000_5000_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_900_1000_4000_5000_6000_7000_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_2500_2600_4500_4600_6500_6600_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_4500_4600_6500_6600_8500_8600_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_500_1000_2500_2600_6000_6500_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_2500_3000_8000_9000_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_4500_5000_6500_6600_both.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_7000_8000_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_6000_8000_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Self")
      assert(chr1_1500_1600_3500_3600_5500_6500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_500_600_1500_1600_3500_4500_5500_6500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1100_1200_1300_1400_3500_3600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2000_3500_4500_5500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_6000_7000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_900_1100_1900_2100_2900_4100_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1500_3500_3600_3700_3800_3900_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_3500_4500_5000_5100_5900_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_900_1100_1900_2100_2900_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1500_3500_3600_3700_3800_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_2000_4500_5000_5100_5900_6000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_3000_3100_3200_3300_4500_4600_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_500_4500_5100_5200_5300_5400_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_3000_3100_3200_3300_4500_5000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_500_5000_5100_5200_5300_5400_plus.minus(chr1_1000_2000_3000_4000_5000_6000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_500_1500_3500_4500_plus.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks overlapping two blocks")
      assert(chr1_1500_2400_2600_3500_plus.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks overlapping two blocks")
      assert(chr1_1500_2500_3500_4500_plus.minus(chr1_1000_2000_3000_4000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks overlapping two blocks")
      assert(chr1_500_600_3000_4000_6500_6600_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks same")
      assert(chr1_1000_2000_2500_6500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks same")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.minus(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks same")
      assert(chr1_900_1100_1900_2100_2900_6000_plus.minus(chr2_900_1100_1900_2100_2900_6000_plus, "name") === Empty, "Different chromosomes")

    }
  }



}
