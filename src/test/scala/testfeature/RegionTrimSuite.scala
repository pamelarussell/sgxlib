package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 10/15/16.
  */
class RegionTrimSuite extends FunSuite {

  test("Empty trim") {
    assert(Empty.trim(5, 6) === Empty)
  }

  test("Block trim") {
    intercept[IllegalArgumentException](chr1_1000_2000_minus.trim(6,5))
    intercept[IllegalArgumentException](chr1_1000_2000_minus.trim(-1, 1))
    assert(chr1_1000_2000_minus.trim(500, 600) === Empty)
    assert(chr1_1000_2000_minus.trim(500, 1000) === Empty)
    assert(chr1_1000_2000_minus.trim(500, 1001) === Block("1", 1000, 1001, Minus))
    assert(chr1_1000_2000_minus.trim(1000, 1010) === Block("1", 1000, 1010, Minus))
    assert(chr1_1000_2000_minus.trim(1100, 1200) === Block("1", 1100, 1200, Minus))
    assert(chr1_1000_2000_minus.trim(1900, 1999) === Block("1", 1900, 1999, Minus))
    assert(chr1_1000_2000_minus.trim(1900, 2000) === Block("1", 1900, 2000, Minus))
    assert(chr1_1000_2000_minus.trim(1900, 2001) === Block("1", 1900, 2000, Minus))
    assert(chr1_1000_2000_minus.trim(1999, 2001) === Block("1", 1999, 2000, Minus))
    assert(chr1_1000_2000_minus.trim(2000, 2001) === Empty)
    assert(chr1_1000_2000_minus.trim(2010, 2020) === Empty)
    assert(chr1_1000_2000_minus.trim(1000, 2000) === chr1_1000_2000_minus)
    assert(chr1_1000_2000_minus.trim(500, 2050) === chr1_1000_2000_minus)
    assert(chr1_1000_2000_minus.trim(500, 2000) === chr1_1000_2000_minus)
    assert(chr1_1000_2000_minus.trim(1000, 2050) === chr1_1000_2000_minus)
  }

  test("BlockSet trim") {
    intercept[IllegalArgumentException](chr1_100_200_300_400_plus.trim(6,5))
    intercept[IllegalArgumentException](chr1_100_200_300_400_plus.trim(-1, 1))
    // Strictly nested both directions
    assert(chr1_900_2100_3000_4000_plus.trim(1000, 2000) ===
      Block("1", 1000, 2000, Plus), "One block of block set completely contains the other block")
    assert(chr1_1100_1200_1300_1400_plus.trim(1000, 2000) === BlockSet(List(
      Block("1", 1100, 1200, Plus),
      Block("1", 1300, 1400, Plus)
    )), "Block completely contains block set")
    // Block set nested inside block sharing one endpoint
    assert(chr1_1000_1100_1200_1300_plus.trim(1000, 2000) === BlockSet(List(
      Block("1", 1000, 1100, Plus),
      Block("1", 1200, 1300, Plus)
    )), "Block completely contains block set, sharing one endpoint")
    // Block set nested inside block sharing two endpoints
    assert(chr1_1000_1100_1900_2000_plus.trim(1000, 2000) === BlockSet(List(
      Block("1", 1000, 1100, Plus),
      Block("1", 1900, 2000, Plus)
    )), "Block completely contains block set, sharing two endpoints")
    assert(chr1_1000_1100_1200_1300_1900_2000_plus.trim(1000, 2000) === BlockSet(List(
      Block("1", 1000, 1100, Plus),
      Block("1", 1200, 1300, Plus),
      Block("1", 1900, 2000, Plus)
    )), "Block completely contains block set, sharing two endpoints")
    // Block nested inside block set sharing one endpoint
    assert(chr1_1000_1100_1200_1300_1900_2000_plus.trim(1000, 1050) ===
      Block("1", 1000, 1050, Plus), "Block nested inside block set sharing one endpoint")
    assert(chr1_1000_1100_1200_1300_1900_2000_plus.trim(1050, 1100) ===
      Block("1", 1050, 1100, Plus), "Block nested inside block set sharing one endpoint")
    assert(chr1_1000_1100_1200_1300_1900_2000_plus.trim(1200, 1250) ===
      Block("1", 1200, 1250, Plus), "Block nested inside block set sharing one endpoint")
    assert(chr1_1000_1100_1200_1300_1900_2000_plus.trim(1250, 1300) ===
      Block("1", 1250, 1300, Plus), "Block nested inside block set sharing one endpoint")
    assert(chr1_1000_1100_1200_1300_1900_2000_plus.trim(1900, 1950) ===
      Block("1", 1900, 1950, Plus), "Block nested inside block set sharing one endpoint")
    assert(chr1_1000_1100_1200_1300_1900_2000_plus.trim(1950, 2000) ===
      Block("1", 1950, 2000, Plus), "Block nested inside block set sharing one endpoint")
    // Overlapping one block off end
    assert(chr1_1900_2100_3000_4000_plus.trim(1000, 2000) === Block("1", 1900, 2000, Plus), "Overlapping one block off end")
    assert(chr1_1900_2100_3000_4000_plus.trim(1500, 2500) === Block("1", 1900, 2100, Plus), "Overlapping one block")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1400, 1550) ===
      Block("1", 1500, 1550, Plus), "Overlapping one block off end")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1550, 1650) ===
      Block("1", 1550, 1600, Plus), "Overlapping one block off end")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1650, 1750) ===
      Block("1", 1700, 1750, Plus), "Overlapping one block off end")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1750, 1850) ===
      Block("1", 1750, 1800, Plus), "Overlapping one block off end")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(2000, 2150) ===
      Block("1", 2100, 2150, Plus), "Overlapping one block off end")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(2200, 2350) ===
      Block("1", 2200, 2300, Plus), "Overlapping one block off end")
    // Overlapping two blocks off end
    assert(chr1_1700_1800_1900_2100_plus.trim(1000, 2000) === BlockSet(List(
      Block("1", 1700, 1800, Plus),
      Block("1", 1900, 2000, Plus)
    )), "Overlapping two blocks off end")
    assert(chr1_1700_1800_1900_2100_plus.trim(1750, 2200) === BlockSet(List(
      Block("1", 1750, 1800, Plus),
      Block("1", 1900, 2100, Plus)
    )), "Overlapping two blocks off end")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1400, 1750) === BlockSet(List(
      Block("1", 1500, 1600, Plus),
      Block("1", 1700, 1750, Plus)
    )), "Overlapping two blocks off end")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1550, 1850) === BlockSet(List(
      Block("1", 1550, 1600, Plus),
      Block("1", 1700, 1800, Plus)
    )), "Overlapping two blocks off end")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1650, 2200) === BlockSet(List(
      Block("1", 1700, 1800, Plus),
      Block("1", 2100, 2200, Plus)
    )), "Overlapping two blocks off end")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1750, 2400) === BlockSet(List(
      Block("1", 1750, 1800, Plus),
      Block("1", 2100, 2300, Plus)
    )), "Overlapping two blocks off end")
    // Overlapping one block and intron
    assert(chr1_1500_1600_2100_2300_plus.trim(1000, 2000) === Block("1", 1500, 1600, Plus), "Overlapping one block and an intron")
    assert(chr1_1500_1600_2100_2300_plus.trim(2000, 3000) === Block("1", 2100, 2300, Plus), "Overlapping one block and an intron")
    // Overlapping two blocks and intron
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1000, 2000) === BlockSet(List(
      Block("1", 1500, 1600, Plus),
      Block("1", 1700, 1800, Plus)
    )), "Overlapping two blocks and an intron")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1650, 2400) === BlockSet(List(
      Block("1", 1700, 1800, Plus),
      Block("1", 2100, 2300, Plus)
    )), "Overlapping two blocks and an intron")
    // Overlapping three blocks and intron
    assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.trim(500, 6500) === BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus)
    )), "Overlapping three blocks and an intron")
    assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.trim(2500, 8500) === BlockSet(List(
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 8000, Plus)
    )), "Overlapping three blocks and an intron")
    // Same span as one block
    assert(chr1_1000_2000_3000_4000_plus.trim(1000, 2000) === Block("1", 1000, 2000, Plus), "Same span as one block")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1500, 1600) ===
      Block("1", 1500, 1600, Plus), "Same span as one block")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1700, 1800) ===
      Block("1", 1700, 1800, Plus), "Same span as one block")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(2100, 2300) ===
      Block("1", 2100, 2300, Plus), "Same span as one block")
    // Contains one block and overlaps two adjacent blocks
    assert(chr1_900_1100_1500_1600_1900_2100_plus.trim(1000, 2000) === BlockSet(List(
      Block("1", 1000, 1100, Plus),
      Block("1", 1500, 1600, Plus),
      Block("1", 1900, 2000, Plus)
    )), "Contains one block and overlaps two adjacent blocks")
    // Contains two blocks and overlaps two adjacent blocks
    assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.trim(1500, 7500) === BlockSet(List(
      Block("1", 1500, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 7500, Plus)
    )), "Contains two blocks and overlaps two adjacent blocks")
    // Contains one block sharing an endpoint
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1400, 1600) ===
      Block("1", 1500, 1600, Plus), "Contains one block sharing an endpoint")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1500, 1650) ===
      Block("1", 1500, 1600, Plus), "Contains one block sharing an endpoint")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1650, 1800) ===
      Block("1", 1700, 1800, Plus), "Contains one block sharing an endpoint")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1700, 1850) ===
      Block("1", 1700, 1800, Plus), "Contains one block sharing an endpoint")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(2000, 2300) ===
      Block("1", 2100, 2300, Plus), "Contains one block sharing an endpoint")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(2100, 2400) ===
      Block("1", 2100, 2300, Plus), "Contains one block sharing an endpoint")
    // Non-overlapping because different spans
    assert(chr1_100_200_300_400_plus.trim(1000, 2000) === Empty, "Non-overlapping because different spans")
    assert(chr1_100_200_300_400_plus.trim(500, 600) === Empty, "Non-overlapping because different spans")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1000, 1100) === Empty, "Non-overlapping because different spans")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(2400, 2500) === Empty, "Non-overlapping because different spans")
    // Same span as an intron
    // Two blocks
    assert(chr1_100_200_300_400_plus.trim(200, 300) === Empty, "Same span as an intron")
    // Three blocks
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1600, 1700) === Empty, "Same span as an intron")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1800, 2100) === Empty, "Same span as an intron")
    // Adjacent to span
    // Two blocks
    assert(chr1_100_200_300_400_plus.trim(50, 100) === Empty, "Adjacent to span")
    assert(chr1_100_200_300_400_plus.trim(400, 500) === Empty, "Adjacent to span")
    // Three blocks
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1400, 1500) === Empty, "Same span as an intron")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(2300, 2400) === Empty, "Same span as an intron")
    // 1bp gap between blocks
    // In intron
    assert(chr1_100_200_300_400_plus.trim(201, 300) === Empty, "1bp gap between blocks")
    assert(chr1_100_200_300_400_plus.trim(200, 299) === Empty, "1bp gap between blocks")
    assert(chr1_100_200_300_400_plus.trim(201, 299) === Empty, "1bp gap between blocks")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1801, 2099) === Empty, "1bp gap between blocks")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1601, 1699) === Empty, "1bp gap between blocks")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1801, 2099) === Empty, "1bp gap between blocks")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1600, 1699) === Empty, "1bp gap between blocks")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1800, 2099) === Empty, "1bp gap between blocks")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1601, 1700) === Empty, "1bp gap between blocks")
    // Outside span
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1400, 1499) === Empty, "1bp gap between blocks")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(2301, 2400) === Empty, "1bp gap between blocks")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(1400, 1500) === Empty, "1bp gap between blocks")
    assert(chr1_1500_1600_1700_1800_2100_2300_plus.trim(2300, 2400) === Empty, "1bp gap between blocks")
  }

}
