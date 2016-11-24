package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 8/30/16.
  */
class RegionMiscSuite extends FunSuite {

  // Run all tests
  adjacencyBlockBlock()
  regionEmptiness()
  regionChr()
  regionStart()
  regionEnd()
  regionOrientation()
  regionGetBlocks()
  regionSize()

  def regionSize(): Unit = {
    test("Region size") {
      assert(Empty.size === 0)
      assert(chr1_500_600_700_800_900_1100_both.size === 400)
      assert(chr1_1000_2000_plus_1.size === 1000)
    }
  }

  def regionNumBlocks(): Unit = {
    test("Number of blocks") {
      assert(Empty.numBlocks === 0, "Empty region should have 0 blocks")
      assert(chr1_5_6_minus.numBlocks === 1, "Block should have 1 block")
      assert(chr1_500_600_700_800_900_1100_both.numBlocks === 3, "Block set with 3 blocks")
    }
  }


  def regionEmptiness(): Unit = {
    test("Region emptiness") {
      assert(Empty.isEmpty, "Empty region should be empty")
      assert(!chr1_1000_2000_plus_1.isEmpty, "Nonempty block should not be empty")
      assert(!chr1_100_200_300_400_plus.isEmpty, "Block set should not be empty")
    }
  }


  def adjacencyBlockBlock(): Unit = {
    test("Block adjacency with block") {
      assert(Block("chr1", 1, 5, Plus).adjacent(Block("chr1", 5, 10, Minus)))
      assert(!Block("chr1", 1, 5, Plus).adjacent(Block("chr1", 6, 10, Plus)))
      assert(!Block("chr1", 1, 5, Plus).adjacent(Block("chr1", 4, 10, Plus)))
      assert(!Block("chr1", 1, 5, Plus).adjacent(Block("chr2", 5, 10, Plus)))
      assert(Block("chr1", 10, 12, Plus).adjacent(Block("chr1", 6, 10, Minus)))
      assert(Block("chr1", 10, 11, Plus).adjacent(Block("chr1", 9, 10, Minus)))
    }
  }


  def regionChr(): Unit = {
    test("Region chr") {
      assert(chr1_1000_2000_both.chr === "chr1")
      assert(chr1_100_200_300_400_plus.chr === "chr1")
      intercept[IllegalStateException](Empty.chr)
    }
  }

  def regionStart(): Unit = {
    test("Region start") {
      assert(chr1_1000_2000_both.start === 1000)
      assert(chr1_100_200_300_400_plus.start === 100)
      intercept[IllegalStateException](Empty.start)
    }
  }

  def regionEnd(): Unit = {
    test("Region end") {
      assert(chr2_1000_2000_plus.end === 2000)
      assert(chr1_100_200_300_400_plus.end === 400)
      intercept[IllegalStateException](Empty.end)
    }
  }

  def regionOrientation(): Unit = {
    test("Region orientation") {
      assert(chr2_1000_2000_plus.orientation === Plus)
      assert(chr1_1000_2000_unstranded.orientation === Unstranded)
      assert(chr1_100_200_300_400_plus.orientation === Plus)
      intercept[IllegalStateException](Empty.orientation)
    }
  }

  def regionGetBlocks(): Unit = {
    test("Region get blocks") {
      assert(chr1_1000_2000_minus.blocks === List(Block("chr1", 1000, 2000, Minus)))
      assert(chr1_100_200_300_400_plus.blocks === List(
        Block("chr1", 100, 200, Plus),
        Block("chr1", 300, 400, Plus)
      ))
      assert(Empty.blocks === Nil)
    }
  }


}
