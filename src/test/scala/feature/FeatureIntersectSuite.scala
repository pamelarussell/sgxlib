package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class FeatureIntersectSuite extends FunSuite {

  intersectionEmpty()
  intersectionBlockBlock()
  intersectionBlockSetBlock()
  intersectionBlockSetBlockSet()

  def intersectionEmpty(): Unit = {
    test("Empty intersection") {
      assert(Empty.intersection(chr1_1000_2000_both, "name") === Empty, "Intersection with Empty should be Empty")
      assert(chr1_1000_2000_unstranded.intersection(Empty, "name") === Empty, "Intersection with Empty should be Empty")
      assert(Empty.intersection(chr1_100_200_300_400_plus, "name") === Empty, "Intersection with Empty should be Empty")
      assert(chr1_100_200_300_400_plus.intersection(Empty, "name") === Empty, "Intersection with Empty should be Empty")
      assert(Empty.intersection(Empty, "name") === Empty, "Empty intersection with Empty should be Empty")
    }
  }

  def intersectionBlockBlock(): Unit = {
    test("Block intersection") {
      // Same span
      assert(chr1_1000_2000_both.intersection(chr1_1000_2000_both, "name") === chr1_1000_2000_both.rename("name"),
        "Intersection of blocks with same span should be the block with consensus orientation")
      assert(chr1_1000_2000_both.intersection(chr1_1000_2000_plus_1, "name") === chr1_1000_2000_plus_1.rename("name"),
        "Intersection of blocks with same span should be the block with consensus orientation")
      assert(chr1_1000_2000_plus_1.intersection(chr1_1000_2000_both, "name") === chr1_1000_2000_plus_1.rename("name"),
        "Intersection of blocks with same span should be the block with consensus orientation")
      // Overlapping
      assert(chr1_1000_2000_both.intersection(chr1_1500_2500_plus, "name") === Block("chr1", 1500, 2000, Plus, "name"),
        "Intersection of overlapping blocks")
      assert(chr1_1500_2500_plus.intersection(chr1_1000_2000_both, "name") === Block("chr1", 1500, 2000, Plus, "name"),
        "Intersection of overlapping blocks")
      // Adjacent
      assert(chr1_1000_2000_plus_1.intersection(chr1_2000_3000_plus, "name") === Empty,
        "Intersection of adjacent blocks should be empty")
      assert(chr1_2000_3000_plus.intersection(chr1_1000_2000_plus_1, "name") === Empty,
        "Intersection of adjacent blocks should be empty")
      // Non-overlapping
      assert(chr2_1000_2000_plus.intersection(chr2_3000_4000_plus, "name") === Empty,
        "Intersection of non-overlapping blocks should be empty")
      assert(chr2_3000_4000_plus.intersection(chr2_1000_2000_plus, "name") === Empty,
        "Intersection of non-overlapping blocks should be empty")
      // Nested
      assert(chr1_1200_1300_plus.intersection(chr1_1000_2000_both, "name") === chr1_1200_1300_plus.rename("name"),
        "Intersection of nested blocks should be inner block with consensus orientation")
      assert(chr1_1000_2000_both.intersection(chr1_1200_1300_plus, "name") === chr1_1200_1300_plus.rename("name"),
        "Intersection of nested blocks should be inner block with consensus orientation")
      // Non-compatible combinations
      assert(chr1_1000_2000_minus.intersection(chr1_1000_2000_plus_1, "name") === Empty,
        "Intersection of non-compatible blocks should be empty")
      assert(chr2_1000_2000_plus.intersection(chr2_3000_4000_plus, "name") === Empty,
        "Intersection of non-overlapping blocks should be empty")
      assert(chr1_1000_2000_plus_1.intersection(chr2_1000_2000_plus, "name") === Empty,
        "Intersection of blocks on different chromosomes should be empty")
    }
  }

  def intersectionBlockSetBlock(): Unit = {
    ignore("BlockSet intersect with Block") {

      /*
      Block set first
       */

      // Strictly nested both directions
      assert(chr1_900_2100_3000_4000_plus.intersection(chr1_1000_2000_both, "name") ===
        Block("chr1", 1000, 2000, Plus, "name"), "One block of block set completely contains the other block")
      assert(chr1_1100_1200_1300_1400_plus.intersection(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1100, 1200, Plus, ""),
        Block("chr1", 1300, 1400, Plus, "")
      ), "name"), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_1100_1200_1300_plus.intersection(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, "")
      ), "name"), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_1100_1900_2000_plus.intersection(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.intersection(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.intersection(Block("chr1", 1000, 1050, Plus, "block"), "name") ===
        Block("chr1", 1000, 1050, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.intersection(Block("chr1", 1050, 1100, Plus, "block"), "name") ===
        Block("chr1", 1050, 1100, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.intersection(Block("chr1", 1200, 1250, Plus, "block"), "name") ===
        Block("chr1", 1200, 1250, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.intersection(Block("chr1", 1250, 1300, Plus, "block"), "name") ===
        Block("chr1", 1250, 1300, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.intersection(Block("chr1", 1900, 1950, Plus, "block"), "name") ===
        Block("chr1", 1900, 1950, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(chr1_1000_1100_1200_1300_1900_2000_plus.intersection(Block("chr1", 1950, 2000, Plus, "block"), "name") ===
        Block("chr1", 1950, 2000, Plus, "name"), "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1900_2100_3000_4000_plus.intersection(chr1_1000_2000_both, "name") === Block("chr1", 1900, 2000, Plus, "name"), "Overlapping one block off end")
      assert(chr1_1900_2100_3000_4000_plus.intersection(chr1_1500_2500_plus, "name") === Block("chr1", 1900, 2100, Plus, "name"), "Overlapping one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1400, 1550, Plus, "block"), "name") ===
        Block("chr1", 1500, 1550, Plus, "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1550, 1650, Plus, "block"), "name") ===
        Block("chr1", 1550, 1600, Plus, "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1650, 1750, Plus, "block"), "name") ===
        Block("chr1", 1700, 1750, Plus, "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1750, 1850, Plus, "block"), "name") ===
        Block("chr1", 1750, 1800, Plus, "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 2000, 2150, Plus, "block"), "name") ===
        Block("chr1", 2100, 2150, Plus, "name"), "Overlapping one block off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 2200, 2350, Plus, "block"), "name") ===
        Block("chr1", 2200, 2300, Plus, "name"), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1700_1800_1900_2100_plus.intersection(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1700_1800_1900_2100_plus.intersection(Block("chr1", 1750, 2200, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1750, 1800, Plus, ""),
        Block("chr1", 1900, 2100, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1400, 1750, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1750, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1550, 1850, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1550, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1650, 2200, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1750, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1750, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1500_1600_2100_2300_plus.intersection(chr1_1000_2000_both, "name") === Block("chr1", 1500, 1600, Plus, "name"), "Overlapping one block and an intron")
      assert(chr1_1500_1600_2100_2300_plus.intersection(chr1_2000_3000_plus, "name") === Block("chr1", 2100, 2300, Plus, "name"), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, "")
      ), "name"), "Overlapping two blocks and an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1650, 2400, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.intersection(Block("chr1", 500, 6500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1000, 2000, Plus, ""),
        Block("chr1", 3000, 4000, Plus, ""),
        Block("chr1", 5000, 6000, Plus, "")
      ), "name"), "Overlapping three blocks and an intron")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.intersection(Block("chr1", 2500, 8500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 3000, 4000, Plus, ""),
        Block("chr1", 5000, 6000, Plus, ""),
        Block("chr1", 7000, 8000, Plus, "")
      ), "name"), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_1000_2000_both, "name") === Block("chr1", 1000, 2000, Plus, "name"), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1500, 1600, Plus, "block"), "name") ===
        Block("chr1", 1500, 1600, Plus, "name"), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1700, 1800, Plus, "block"), "name") ===
        Block("chr1", 1700, 1800, Plus, "name"), "Same span as one block")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 2100, 2300, Plus, "block"), "name") ===
        Block("chr1", 2100, 2300, Plus, "name"), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_900_1100_1500_1600_1900_2100_plus.intersection(chr1_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.intersection(Block("chr1", 1500, 7500, Plus, "block"), "name") === BlockSet(List(
        Block("chr1", 1500, 2000, Plus, ""),
        Block("chr1", 3000, 4000, Plus, ""),
        Block("chr1", 5000, 6000, Plus, ""),
        Block("chr1", 7000, 7500, Plus, "")
      ), "name"), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1400, 1600, Plus, "block"), "name") ===
        Block("chr1", 1500, 1600, Plus, "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1500, 1650, Plus, "block"), "name") ===
        Block("chr1", 1500, 1600, Plus, "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1650, 1800, Plus, "block"), "name") ===
        Block("chr1", 1700, 1800, Plus, "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1700, 1850, Plus, "block"), "name") ===
        Block("chr1", 1700, 1800, Plus, "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 2000, 2300, Plus, "block"), "name") ===
        Block("chr1", 2100, 2300, Plus, "name"), "Contains one block sharing an endpoint")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 2100, 2400, Plus, "block"), "name") ===
        Block("chr1", 2100, 2300, Plus, "name"), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_100_200_300_400_plus.intersection(chr1_1000_2000_plus_1, "name") === Empty, "Non-overlapping because different spans")
      assert(chr1_100_200_300_400_plus.intersection(Block("chr1", 500, 600, Both, "block"), "name") === Empty, "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1000, 1100, Both, "block"), "name") === Empty, "Non-overlapping because different spans")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 2400, 2500, Both, "block"), "name") === Empty, "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(chr1_1000_2000_minus, "name") === Empty, "Non-overlapping because different orientations")
      // Different chromosomes
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.intersection(chr2_1000_2000_plus, "name") === Empty, "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(chr1_100_200_300_400_plus.intersection(Block("chr1", 200, 300, Plus, "block"), "name") === Empty, "Same span as an intron")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1600, 1700, Plus, "block"), "name") === Empty, "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1800, 2100, Plus, "block"), "name") === Empty, "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(chr1_100_200_300_400_plus.intersection(Block("chr1", 50, 100, Plus, "block"), "name") === Empty, "Adjacent to span")
      assert(chr1_100_200_300_400_plus.intersection(Block("chr1", 400, 500, Plus, "block"), "name") === Empty, "Adjacent to span")
      // Three blocks
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1400, 1500, Plus, "block"), "name") === Empty, "Same span as an intron")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 2300, 2400, Plus, "block"), "name") === Empty, "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(chr1_100_200_300_400_plus.intersection(Block("chr1", 201, 300, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.intersection(Block("chr1", 200, 299, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_100_200_300_400_plus.intersection(Block("chr1", 201, 299, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1801, 2099, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1601, 1699, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1801, 2099, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1600, 1699, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1800, 2099, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1601, 1700, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      // Outside span
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1400, 1499, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 2301, 2400, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 1400, 1500, Plus, "block"), "name") === Empty, "1bp gap between blocks")
      assert(chr1_1500_1600_1700_1800_2100_2300_plus.intersection(Block("chr1", 2300, 2400, Plus, "block"), "name") === Empty, "1bp gap between blocks")

      /*
      Block first
       */

      // Strictly nested both directions
      assert(chr1_1000_2000_both.intersection(chr1_900_2100_3000_4000_plus, "name") ===
        Block("chr1", 1000, 2000, Plus, "name"), "One block of block set completely contains the other block")
      assert(chr1_1000_2000_both.intersection(chr1_1100_1200_1300_1400_plus, "name") === BlockSet(List(
        Block("chr1", 1100, 1200, Plus, ""),
        Block("chr1", 1300, 1400, Plus, "")
      ), "name"), "Block completely contains block set")
      // Block set nested inside block sharing one endpoint
      assert(chr1_1000_2000_both.intersection(chr1_1000_1100_1200_1300_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, "")
      ), "name"), "Block completely contains block set, sharing one endpoint")
      // Block set nested inside block sharing two endpoints
      assert(chr1_1000_2000_both.intersection(chr1_1000_1100_1900_2000_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block completely contains block set, sharing two endpoints")
      assert(chr1_1000_2000_both.intersection(chr1_1000_1100_1200_1300_1900_2000_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1200, 1300, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Block completely contains block set, sharing two endpoints")
      // Block nested inside block set sharing one endpoint
      assert(Block("chr1", 1000, 1050, Plus, "block").intersection(chr1_1000_1100_1200_1300_1900_2000_plus, "name") ===
        Block("chr1", 1000, 1050, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1050, 1100, Plus, "block").intersection(chr1_1000_1100_1200_1300_1900_2000_plus, "name") ===
        Block("chr1", 1050, 1100, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1200, 1250, Plus, "block").intersection(chr1_1000_1100_1200_1300_1900_2000_plus, "name") ===
        Block("chr1", 1200, 1250, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1250, 1300, Plus, "block").intersection(chr1_1000_1100_1200_1300_1900_2000_plus, "name") ===
        Block("chr1", 1250, 1300, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1900, 1950, Plus, "block").intersection(chr1_1000_1100_1200_1300_1900_2000_plus, "name") ===
        Block("chr1", 1900, 1950, Plus, "name"), "Block nested inside block set sharing one endpoint")
      assert(Block("chr1", 1950, 2000, Plus, "block").intersection(chr1_1000_1100_1200_1300_1900_2000_plus, "name") ===
        Block("chr1", 1950, 2000, Plus, "name"), "Block nested inside block set sharing one endpoint")
      // Overlapping one block off end
      assert(chr1_1000_2000_both.intersection(chr1_1900_2100_3000_4000_plus, "name") === Block("chr1", 1900, 2000, Plus, "name"), "Overlapping one block off end")
      assert(chr1_1500_2500_plus.intersection(chr1_1900_2100_3000_4000_plus, "name") === Block("chr1", 1900, 2100, Plus, "name"), "Overlapping one block")
      assert(Block("chr1", 1400, 1550, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1500, 1550, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 1550, 1650, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1550, 1600, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 1650, 1750, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1700, 1750, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 1750, 1850, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1750, 1800, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 2000, 2150, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2100, 2150, Plus, "name"), "Overlapping one block off end")
      assert(Block("chr1", 2200, 2350, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2200, 2300, Plus, "name"), "Overlapping one block off end")
      // Overlapping two blocks off end
      assert(chr1_1000_2000_both.intersection(chr1_1700_1800_1900_2100_plus, "name") === BlockSet(List(
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1750, 2200, Plus, "block").intersection(chr1_1700_1800_1900_2100_plus, "name") === BlockSet(List(
        Block("chr1", 1750, 1800, Plus, ""),
        Block("chr1", 1900, 2100, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1400, 1750, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1750, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1550, 1850, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1550, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1650, 2200, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      assert(Block("chr1", 1750, 2400, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1750, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks off end")
      // Overlapping one block and intron
      assert(chr1_1000_2000_both.intersection(chr1_1500_1600_2100_2300_plus, "name") === Block("chr1", 1500, 1600, Plus, "name"), "Overlapping one block and an intron")
      assert(chr1_2000_3000_plus.intersection(chr1_1500_1600_2100_2300_plus, "name") === Block("chr1", 2100, 2300, Plus, "name"), "Overlapping one block and an intron")
      // Overlapping two blocks and intron
      assert(chr1_1000_2000_both.intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1700, 1800, Plus, "")
      ), "name"), "Overlapping two blocks and an intron")
      assert(Block("chr1", 1650, 2400, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === BlockSet(List(
        Block("chr1", 1700, 1800, Plus, ""),
        Block("chr1", 2100, 2300, Plus, "")
      ), "name"), "Overlapping two blocks and an intron")
      // Overlapping three blocks and intron
      assert(Block("chr1", 500, 6500, Plus, "block").intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 2000, Plus, ""),
        Block("chr1", 3000, 4000, Plus, ""),
        Block("chr1", 5000, 6000, Plus, "")
      ), "name"), "Overlapping three blocks and an intron")
      assert(Block("chr1", 2500, 8500, Plus, "block").intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", 3000, 4000, Plus, ""),
        Block("chr1", 5000, 6000, Plus, ""),
        Block("chr1", 7000, 8000, Plus, "")
      ), "name"), "Overlapping three blocks and an intron")
      // Same span as one block
      assert(chr1_1000_2000_both.intersection(chr1_1000_2000_3000_4000_plus, "name") === Block("chr1", 1000, 2000, Plus, "name"), "Same span as one block")
      assert(Block("chr1", 1500, 1600, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1500, 1600, Plus, "name"), "Same span as one block")
      assert(Block("chr1", 1700, 1800, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1700, 1800, Plus, "name"), "Same span as one block")
      assert(Block("chr1", 2100, 2300, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2100, 2300, Plus, "name"), "Same span as one block")
      // Contains one block and overlaps two adjacent blocks
      assert(chr1_1000_2000_both.intersection(chr1_900_1100_1500_1600_1900_2100_plus, "name") === BlockSet(List(
        Block("chr1", 1000, 1100, Plus, ""),
        Block("chr1", 1500, 1600, Plus, ""),
        Block("chr1", 1900, 2000, Plus, "")
      ), "name"), "Contains one block and overlaps two adjacent blocks")
      // Contains two blocks and overlaps two adjacent blocks
      assert(Block("chr1", 1500, 7500, Plus, "block").intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", 1500, 2000, Plus, ""),
        Block("chr1", 3000, 4000, Plus, ""),
        Block("chr1", 5000, 6000, Plus, ""),
        Block("chr1", 7000, 7500, Plus, "")
      ), "name"), "Contains two blocks and overlaps two adjacent blocks")
      // Contains one block sharing an endpoint
      assert(Block("chr1", 1400, 1600, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1500, 1600, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 1500, 1650, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1500, 1600, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 1650, 1800, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1700, 1800, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 1700, 1850, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 1700, 1800, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 2000, 2300, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2100, 2300, Plus, "name"), "Contains one block sharing an endpoint")
      assert(Block("chr1", 2100, 2400, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") ===
        Block("chr1", 2100, 2300, Plus, "name"), "Contains one block sharing an endpoint")
      // Non-overlapping because different spans
      assert(chr1_1000_2000_plus_1.intersection(chr1_100_200_300_400_plus, "name") === Empty, "Non-overlapping because different spans")
      assert(Block("chr1", 500, 600, Both, "block").intersection(chr1_100_200_300_400_plus, "name") === Empty, "Non-overlapping because different spans")
      assert(Block("chr1", 1000, 1100, Both, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Non-overlapping because different spans")
      assert(Block("chr1", 2400, 2500, Both, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Non-overlapping because different spans")
      // Non-overlapping because different orientations
      assert(chr1_1000_2000_minus.intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Non-overlapping because different orientations")
      // Different chromosomes
      assert(chr2_1000_2000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, "name") === Empty, "Different chromosomes")
      // Same span as an intron
      // Two blocks
      assert(Block("chr1", 200, 300, Plus, "block").intersection(chr1_100_200_300_400_plus, "name") === Empty, "Same span as an intron")
      // Three blocks
      assert(Block("chr1", 1600, 1700, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Same span as an intron")
      assert(Block("chr1", 1800, 2100, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Same span as an intron")
      // Adjacent to span
      // Two blocks
      assert(Block("chr1", 50, 100, Plus, "block").intersection(chr1_100_200_300_400_plus, "name") === Empty, "Adjacent to span")
      assert(Block("chr1", 400, 500, Plus, "block").intersection(chr1_100_200_300_400_plus, "name") === Empty, "Adjacent to span")
      // Three blocks
      assert(Block("chr1", 1400, 1500, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Same span as an intron")
      assert(Block("chr1", 2300, 2400, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "Same span as an intron")
      // 1bp gap between blocks
      // In intron
      assert(Block("chr1", 201, 300, Plus, "block").intersection(chr1_100_200_300_400_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 200, 299, Plus, "block").intersection(chr1_100_200_300_400_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 201, 299, Plus, "block").intersection(chr1_100_200_300_400_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 1801, 2099, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 1601, 1699, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 1801, 2099, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 1600, 1699, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 1800, 2099, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 1601, 1700, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")
      // Outside span
      assert(Block("chr1", 1400, 1499, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 2301, 2400, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 1400, 1500, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")
      assert(Block("chr1", 2300, 2400, Plus, "block").intersection(chr1_1500_1600_1700_1800_2100_2300_plus, "name") === Empty, "1bp gap between blocks")

    }
  }

  def intersectionBlockSetBlockSet(): Unit = {
    ignore("BlockSet intersection with BlockSet") {

      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1500_1600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_1500_1600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2500_2600_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2500_2600_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_3500_3600_4500_4600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_3500_3600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_5500_5600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_5500_5600_6500_6600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_1500_1600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_1000_1500_1600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_1000_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_1000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2000_3000_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2500_3000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_3000_3500_3600_4500_4600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_2600_3500_3600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_3000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_4000_4600_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_4000_5000_5500_5600_6000_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_5500_5600_6000_6600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_500_600_3500_3600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_1100_1200_2500_3000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_1100_1200_4000_4600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_2000_3000_3500_3600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block nested, others non-overlapping")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_900_1100_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_900_2000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_500_600_900_1100_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_500_600_1000_2000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_500_600_900_2000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping off end of span")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_2500_4000_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_3500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_4000_4500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_2700_3500_6000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_3500_4000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_4000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2000_5000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_5000_6000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_5000_7000_8000_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_4500_4600_5500_6500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_3000_4000_5000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2100_2200_2300_2400_2500_3100_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2100_2200_2300_3000_4000_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1500_3500_3600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1000_2000_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_4500_5500_5600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1500_2500_2600_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_3500_3600_3800_4500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_3600_4500_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_3000_3500_3600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1000_3000_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_4500_5500_5600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1500_2500_2600_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_2600_3500_3600_3800_4500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_3600_4000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_6500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_5500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_5500_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_3500_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_2600_2800_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_8500_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_5500_6000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_1500_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_1500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_5500_6500_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_2000_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_2600_2800_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_8500_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping three blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_4500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_3500_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_5500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_3500_4500_4600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_4500_5500_6500_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_4500_5500_6500_7500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_4500_6500_8500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_5000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_5000_5500_5600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_5500_6000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_4000_4500_4600_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_5000_5500_6500_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_4500_6000_6500_7500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_4500_6500_8500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block overlapping two blocks of other")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1000_2000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_4000_5500_6500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_4500_4600_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_2500_3500_4500_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_5000_6000_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_3000_4500_4600_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_2500_3000_4500_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same blocks compatible orientation")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_7000_8000_9000_10000_minus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, different orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_1000_4000_7000_8000_minus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, different orientation, overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_6000_7000_8000_9000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, adjacent spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_2000_3000_4000_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_3000_4000_5000_6000_7000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_3000_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_3000_4000_5000_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_900_1000_4000_5000_6000_7000_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_2600_4500_4600_6500_6600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_4500_4600_6500_6600_8500_8600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1000_2500_2600_6000_6500_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_3000_8000_9000_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_4500_5000_6500_6600_both, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_7000_8000_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_6000_8000_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Self")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_3500_3600_5500_6500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_1500_1600_3500_4500_5500_6500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1100_1200_1300_1400_3500_3600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks nested in two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1500_2500_3500_4500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1500_2000_3500_4500_5500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1500_2500_3500_4500_5500_6000_7000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping three blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_900_1100_1900_2100_2900_4100_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_3500_3600_3700_3800_3900_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_4500_5000_5100_5900_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_900_1100_1900_2100_2900_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_3500_3600_3700_3800_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_4500_5000_5100_5900_6000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_3100_3200_3300_4500_4600_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_4500_5100_5200_5300_5400_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_3100_3200_3300_4500_5000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_5000_5100_5200_5300_5400_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks nested in one block")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_500_1500_3500_4500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_1500_2400_2600_3500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_plus.intersection(chr1_1500_2500_3500_4500_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks overlapping two blocks")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_3000_4000_6500_6600_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1000_2000_2500_6500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks same")
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus, "name") === BlockSet(List(
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, ""),
        Block("chr1", ???, ???, ???, "")
      ), "name"), "Two blocks same")
      assert(chr1_500_600_1500_1600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1500_1600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_1500_1600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_1500_1600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_3500_3600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_3500_3600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_5500_5600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_5500_5600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_2600_3500_3600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2500_2600_3500_3600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_2600_5500_5600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2500_2600_5500_5600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_3500_3600_4500_4600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_3500_3600_4500_4600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_3500_3600_6500_6600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_3500_3600_6500_6600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_2500_2600_5500_5600_6500_6600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_5500_5600_6500_6600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_5500_5600_6500_6600_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_5500_5600_6500_6600_7000_8000_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_1000_1500_1600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_1500_1600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_1500_1600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_1000_1500_1600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_3500_3600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_1000_3500_3600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_1000_5500_5600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_1000_5500_5600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_2000_3000_3500_3600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2000_3000_3500_3600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_2500_3000_5500_5600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2500_3000_5500_5600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_2000_3000_3500_3600_4500_4600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_3000_3500_3600_4500_4600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_2000_2600_3500_3600_6500_6600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_2600_3500_3600_6500_6600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_2500_3000_5500_5600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_3000_5500_5600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_4000_4600_5500_5600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_4000_4600_5500_5600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_4000_5000_5500_5600_6000_6600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_4000_5000_5500_5600_6000_6600_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_5500_5600_6000_6600_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_5500_5600_6000_6600_7000_8000_plus, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_3500_3600_both.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_500_600_3500_3600_both, "name"), "One block nested, others non-overlapping")
      assert(chr1_1100_1200_2500_3000_both.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_1100_1200_2500_3000_both, "name"), "One block nested, others non-overlapping")
      assert(chr1_1100_1200_4000_4600_both.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_1100_1200_4000_4600_both, "name"), "One block nested, others non-overlapping")
      assert(chr1_2000_3000_3500_3600_both.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_2000_3000_3500_3600_both, "name"), "One block nested, others non-overlapping")
      assert(chr1_500_600_700_800_900_1100_both.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_900_1100_both, "name"), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_1000_2000_both.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_1000_2000_both, "name"), "One block overlapping off end of span")
      assert(chr1_500_600_700_800_900_2000_both.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_700_800_900_2000_both, "name"), "One block overlapping off end of span")
      assert(chr1_500_600_900_1100_both.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_500_600_900_1100_both, "name"), "One block overlapping off end of span")
      assert(chr1_500_600_1000_2000_both.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_500_600_1000_2000_both, "name"), "One block overlapping off end of span")
      assert(chr1_500_600_900_2000_both.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_500_600_900_2000_both, "name"), "One block overlapping off end of span")
      assert(chr1_1500_2500_4000_5000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_2500_4000_5000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_2500_3500_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_3500_7000_8000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_3000_4000_4500_5500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_4000_4500_5500_plus, "name"), "One block overlapping one block of other")
      assert(chr1_2500_2600_2700_3500_6000_7000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_2700_3500_6000_7000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_500_600_3500_4000_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_3500_4000_7000_8000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_500_600_4000_6000_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_4000_6000_7000_8000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_500_600_2000_5000_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_2000_5000_7000_8000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_3500_5000_6000_7000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_5000_6000_7000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_5000_6000_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_5000_6000_7000_8000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_500_1000_5000_7000_8000_9000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_5000_7000_8000_9000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_4500_4600_5500_6500_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_4500_4600_5500_6500_7000_8000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_2000_3000_4000_5000_5500_5600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_3000_4000_5000_5500_5600_plus, "name"), "One block overlapping one block of other")
      assert(chr1_2100_2200_2300_2400_2500_3100_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2100_2200_2300_2400_2500_3100_plus, "name"), "One block overlapping one block of other")
      assert(chr1_2100_2200_2300_3000_4000_5000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2100_2200_2300_3000_4000_5000_plus, "name"), "One block overlapping one block of other")
      assert(chr1_500_1500_3500_3600_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1500_3500_3600_7000_8000_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_2000_3500_3600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1000_2000_3500_3600_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_4500_5500_5600_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_4500_5500_5600_7000_8000_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1500_2500_2600_3500_3600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1500_2500_2600_3500_3600_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_2500_2600_3500_3600_3800_4500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_3500_3600_3800_4500_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_3600_4500_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_3600_4500_6000_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_3000_3500_3600_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_3000_3500_3600_7000_8000_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_1000_3000_3500_3600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1000_3000_3500_3600_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_3000_4500_5500_5600_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_4500_5500_5600_7000_8000_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1500_2500_2600_3500_3600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1500_2500_2600_3500_3600_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_2000_2600_3500_3600_3800_4500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_2600_3500_3600_3800_4500_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_3500_3600_4000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_3600_4000_6000_plus, "name"), "One block overlapping one block, another block nested in another block")
      assert(chr1_500_6500_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_6500_7000_8000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_1500_5500_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_5500_7000_8000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_500_600_1000_7000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1000_7000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_500_600_1500_5500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1500_5500_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_500_5500_6500_6600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_5500_6500_6600_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_500_600_3500_9000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_3500_9000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_2500_2600_2800_9000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_2600_2800_9000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_2500_8500_9000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_8500_9000_10000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_1500_5500_6000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_5500_6000_8000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_500_1000_1500_7000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_1500_7000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_500_1000_1500_5500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_1000_1500_5500_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_500_5500_6500_7000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_5500_6500_7000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_500_600_2000_9000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_2000_9000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_2000_2600_2800_9000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_2600_2800_9000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_2000_8500_9000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_8500_9000_10000_plus, "name"), "One block overlapping three blocks of other")
      assert(chr1_500_4500_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_4500_7000_8000_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_3500_5500_5600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_3500_5500_5600_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_3500_5500_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_5500_7000_8000_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_3000_6000_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_6000_7000_8000_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_500_3500_4500_4600_6500_6600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_3500_4500_4600_6500_6600_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_3500_4500_5500_6500_9000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_4500_5500_6500_9000_10000_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_4500_5500_6500_7500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_4500_5500_6500_7500_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_4500_6500_8500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_4500_6500_8500_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_500_5000_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_5000_7000_8000_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_5000_5500_5600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_5000_5500_5600_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_3500_5500_6000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3500_5500_6000_8000_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_2000_6000_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2000_6000_7000_8000_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_500_4000_4500_4600_6500_6600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_4000_4500_4600_6500_6600_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_3500_5000_5500_6500_9000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_5000_5500_6500_9000_10000_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_4500_6000_6500_7500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_4500_6000_6500_7500_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_2000_4500_6500_8500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_4500_6500_8500_plus, "name"), "One block overlapping two blocks of other")
      assert(chr1_500_600_1000_2000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_600_1000_2000_plus, "name"), "One block same")
      assert(chr1_3000_4000_5500_6500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_4000_5500_6500_plus, "name"), "One block same")
      assert(chr1_2500_2600_4500_4600_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_2600_4500_4600_5000_6000_plus, "name"), "One block same")
      assert(chr1_1500_2500_3500_4500_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_2500_3500_4500_5000_6000_plus, "name"), "One block same")
      assert(chr1_5000_6000_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_5000_6000_7000_8000_plus, "name"), "One block same")
      assert(chr1_2500_3000_4500_4600_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_2500_3000_4500_4600_5000_6000_plus, "name"), "One block same")
      assert(chr1_1500_2500_3000_4500_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1500_2500_3000_4500_5000_6000_plus, "name"), "One block same")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name"), "Same blocks compatible orientation")
      assert(chr1_7000_8000_9000_10000_minus.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_7000_8000_9000_10000_minus, "name"), "Same chromosome, different orientation, non-overlapping spans")
      assert(chr1_1000_4000_7000_8000_minus.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_1000_4000_7000_8000_minus, "name"), "Same chromosome, different orientation, overlapping spans")
      assert(chr1_6000_7000_8000_9000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_6000_7000_8000_9000_plus, "name"), "Same chromosome, same orientation, adjacent spans")
      assert(chr1_2000_3000_4000_5000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_2000_3000_4000_5000_plus, "name"), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_2000_3000_4000_5000_6000_7000_both.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_3000_4000_5000_6000_7000_both, "name"), "Same chromosome, same orientation, interleaved exons completely filling span")
      assert(chr1_2000_3000_8000_9000_both.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_3000_8000_9000_both, "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_2000_3000_4000_5000_8000_9000_both.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_3000_4000_5000_8000_9000_both, "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_900_1000_4000_5000_6000_7000_8000_9000_both.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_900_1000_4000_5000_6000_7000_8000_9000_both, "name"), "Same chromosome, same orientation, interleaved exons filling some introns not all")
      assert(chr1_2500_2600_4500_4600_6500_6600_both.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2500_2600_4500_4600_6500_6600_both, "name"), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_4500_4600_6500_6600_8500_8600_both.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_4500_4600_6500_6600_8500_8600_both, "name"), "Same chromosome, same orientation, interleaved exons none adjacent")
      assert(chr1_500_1000_2500_2600_6000_6500_8000_9000_both.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1000_2500_2600_6000_6500_8000_9000_both, "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_4500_5000_6500_6600_both.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_4500_5000_6500_6600_both, "name"), "Same chromosome, same orientation, interleaved exons some adjacent")
      assert(chr1_7000_8000_9000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_7000_8000_9000_10000_plus, "name"), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_6000_8000_9000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_6000_8000_9000_10000_plus, "name"), "Same chromosome, same orientation, non-overlapping spans")
      assert(chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_plus, "name"), "Self")
      assert(chr1_1500_1600_3500_3600_5500_6500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_3500_3600_5500_6500_plus, "name"), "Three blocks nested in three blocks")
      assert(chr1_500_600_1500_1600_3500_4500_5500_6500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_1500_1600_3500_4500_5500_6500_plus, "name"), "Three blocks nested in three blocks")
      assert(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus, "name"), "Three blocks nested in three blocks")
      assert(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus, "name"), "Three blocks nested in three blocks")
      assert(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1000_1500_1600_3500_3600_5500_6500_plus, "name"), "Three blocks nested in three blocks")
      assert(chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus, "name"), "Three blocks nested in three blocks")
      assert(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus, "name"), "Three blocks nested in three blocks")
      assert(chr1_1100_1200_1300_1400_3500_3600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1100_1200_1300_1400_3500_3600_plus, "name"), "Three blocks nested in two blocks")
      assert(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus, "name"), "Three blocks nested in two blocks")
      assert(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus, "name"), "Three blocks nested in two blocks")
      assert(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus, "name"), "Three blocks nested in two blocks")
      assert(chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus, "name"), "Three blocks nested in two blocks")
      assert(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus, "name"), "Three blocks nested in two blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1500_2500_3500_4500_5500_plus, "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1500_2500_3500_4500_5500_6500_6600_plus, "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus, "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2000_3500_4500_5500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1500_2000_3500_4500_5500_plus, "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_1500_2500_3500_4500_5500_6000_7000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_1500_2500_3500_4500_5500_6000_7000_plus, "name"), "Three blocks overlapping three blocks")
      assert(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus, "name"), "Three blocks overlapping three blocks")
      assert(chr1_900_1100_1900_2100_2900_4100_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_900_1100_1900_2100_2900_4100_plus, "name"), "Three blocks overlapping two blocks")
      assert(chr1_1500_3500_3600_3700_3800_3900_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_3500_3600_3700_3800_3900_plus, "name"), "Three blocks overlapping two blocks")
      assert(chr1_3500_4500_5000_5100_5900_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_3500_4500_5000_5100_5900_6000_plus, "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus, "name"), "Three blocks overlapping two blocks")
      assert(chr1_900_1100_1900_2100_2900_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_900_1100_1900_2100_2900_6000_plus, "name"), "Three blocks overlapping two blocks")
      assert(chr1_1500_3500_3600_3700_3800_5000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1500_3500_3600_3700_3800_5000_plus, "name"), "Three blocks overlapping two blocks")
      assert(chr1_2000_4500_5000_5100_5900_6000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_2000_4500_5000_5100_5900_6000_plus, "name"), "Three blocks overlapping two blocks")
      assert(chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus, "name"), "Three blocks overlapping two blocks")
      assert(chr1_3000_3100_3200_3300_4500_4600_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_3100_3200_3300_4500_4600_plus, "name"), "Two blocks nested in one block")
      assert(chr1_500_4500_5100_5200_5300_5400_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_4500_5100_5200_5300_5400_plus, "name"), "Two blocks nested in one block")
      assert(chr1_3000_3100_3200_3300_4500_5000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_3000_3100_3200_3300_4500_5000_plus, "name"), "Two blocks nested in one block")
      assert(chr1_500_5000_5100_5200_5300_5400_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_both.intersection(chr1_500_5000_5100_5200_5300_5400_plus, "name"), "Two blocks nested in one block")
      assert(chr1_500_1500_3500_4500_plus.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_500_1500_3500_4500_plus, "name"), "Two blocks overlapping two blocks")
      assert(chr1_1500_2400_2600_3500_plus.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_1500_2400_2600_3500_plus, "name"), "Two blocks overlapping two blocks")
      assert(chr1_1500_2500_3500_4500_plus.intersection(chr1_1000_2000_3000_4000_plus, "name") ===
        chr1_1000_2000_3000_4000_plus.intersection(chr1_1500_2500_3500_4500_plus, "name"), "Two blocks overlapping two blocks")
      assert(chr1_500_600_3000_4000_6500_6600_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_500_600_3000_4000_6500_6600_7000_8000_plus, "name"), "Two blocks same")
      assert(chr1_1000_2000_2500_6500_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1000_2000_2500_6500_7000_8000_plus, "name"), "Two blocks same")
      assert(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus.intersection(chr1_1000_2000_3000_4000_5000_6000_7000_8000_both, "name") ===
        chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.intersection(chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus, "name"), "Two blocks same")
    }
  }



}
