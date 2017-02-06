package testfeature

import feature._
import org.scalatest.FunSuite

/**
  * Created by prussell on 8/30/16.
  */
class RegionMiscSuite extends FunSuite {

  // Run all tests
  regionEmptiness()
  regionChr()
  regionStart()
  regionEnd()
  regionOrientation()
  regionGetBlocks()
  regionSize()
  regionRelativePos()
  regionChrPos()
  regionToString()
  regionGetIntrons()

  def regionGetIntrons(): Unit = {
    test("Get introns") {
      assert(chr1_1000_2000_minus
        .getIntrons === Nil)
      assert(chr1_100_200_300_400_plus
        .getIntrons === List(Block("1", 200, 300, Plus)))
      assert(chr1_100_200_300_400_minus
        .getIntrons === List(Block("1", 200, 300, Minus)))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus
        .getIntrons === List(
        Block("1", 2000, 3000, Plus),
        Block("1", 4000, 5000, Plus),
        Block("1", 6000, 7000, Plus)))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus
        .getIntrons === List(
        Block("1", 2000, 3000, Minus),
        Block("1", 4000, 5000, Minus),
        Block("1", 6000, 7000, Minus)))
    }
  }


  def regionToString(): Unit = {
    test("Region toString") {
      assert(Empty.toString === "Empty")
    }
  }

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

  def regionChr(): Unit = {
    test("Region chr") {
      assert(chr1_1000_2000_both.chr === "1")
      assert(chr1_100_200_300_400_plus.chr === "1")
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
      assert(chr1_1000_2000_both.orientation === Unstranded)
      assert(chr1_100_200_300_400_plus.orientation === Plus)
      intercept[IllegalStateException](Empty.orientation)
    }
  }

  def regionGetBlocks(): Unit = {
    test("Region get blocks") {
      assert(chr1_1000_2000_minus.blocks === List(Block("1", 1000, 2000, Minus)))
      assert(chr1_100_200_300_400_plus.blocks === List(
        Block("1", 100, 200, Plus),
        Block("1", 300, 400, Plus)
      ))
      assert(Empty.blocks === Nil)
    }
  }

  def regionRelativePos(): Unit = {
    test("Region relative position") {
      assert(Empty.relativePos(10) === None)
      intercept[IllegalArgumentException](chr1_1000_2000_both.relativePos(10))
      intercept[IllegalArgumentException](chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.relativePos(10))
      assert(chr1_1500_2500_plus.relativePos(1499) === None)
      assert(chr1_1500_2500_plus.relativePos(1500) === Some(0))
      assert(chr1_1500_2500_plus.relativePos(2000) === Some(500))
      assert(chr1_1500_2500_plus.relativePos(2499) === Some(999))
      assert(chr1_1500_2500_plus.relativePos(2500) === None)
      assert(chr1_1500_2500_plus.relativePos(2600) === None)
      assert(chr1_1500_2500_minus.relativePos(1499) === None)
      assert(chr1_1500_2500_minus.relativePos(1500) === Some(999))
      assert(chr1_1500_2500_minus.relativePos(2000) === Some(499))
      assert(chr1_1500_2500_minus.relativePos(2499) === Some(0))
      assert(chr1_1500_2500_minus.relativePos(2500) === None)
      assert(chr1_1500_2500_minus.relativePos(2600) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(500) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(999) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(1000) === Some(0))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(1500) === Some(500))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(1999) === Some(999))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(2000) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(2500) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(2999) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(3000) === Some(1000))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(3500) === Some(1500))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(3999) === Some(1999))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(4000) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(6500) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(6999) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(7000) === Some(3000))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(7500) === Some(3500))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(7999) === Some(3999))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(8000) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.relativePos(8500) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(500) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(999) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(1000) === Some(3999))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(1500) === Some(3499))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(1999) === Some(3000))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(2000) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(2500) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(2999) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(3000) === Some(2999))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(3500) === Some(2499))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(3999) === Some(2000))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(4000) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(6500) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(6999) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(7000) === Some(999))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(7500) === Some(499))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(7999) === Some(0))
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(8000) === None)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.relativePos(8500) === None)
    }
  }

  def regionChrPos(): Unit = {
    test("Region chr position") {
      intercept[IllegalStateException](Empty.chrPos(10))
      intercept[IllegalArgumentException](chr1_1000_2000_both.chrPos(10))
      intercept[IllegalArgumentException](chr1_1000_2000_3000_4000_5000_6000_7000_8000_both.chrPos(10))
      intercept[IllegalArgumentException](chr1_1500_2500_plus.chrPos(-1))
      intercept[IllegalArgumentException](chr1_1500_2500_minus.chrPos(-1))
      intercept[IllegalArgumentException](chr1_1500_2500_plus.chrPos(1000))
      intercept[IllegalArgumentException](chr1_1500_2500_minus.chrPos(1000))
      intercept[IllegalArgumentException](chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(-1))
      intercept[IllegalArgumentException](chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(-1))
      intercept[IllegalArgumentException](chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(4000))
      intercept[IllegalArgumentException](chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(4000))
      assert(chr1_1500_2500_plus.chrPos(0) === 1500)
      assert(chr1_1500_2500_plus.chrPos(500) === 2000)
      assert(chr1_1500_2500_plus.chrPos(999) === 2499)
      assert(chr1_1500_2500_minus.chrPos(0) === 2499)
      assert(chr1_1500_2500_minus.chrPos(500) === 1999)
      assert(chr1_1500_2500_minus.chrPos(999) === 1500)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(0) === 1000)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(500) === 1500)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(999) === 1999)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(1000) === 3000)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(1500) === 3500)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(1999) === 3999)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(2000) === 5000)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(2999) === 5999)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(3000) === 7000)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(3500) === 7500)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus.chrPos(3999) === 7999)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(0) === 7999)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(500) === 7499)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(999) === 7000)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(1000) === 5999)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(1500) === 5499)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(1999) === 5000)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(2000) === 3999)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(2999) === 3000)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(3000) === 1999)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(3500) === 1499)
      assert(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus.chrPos(3999) === 1000)
    }
  }


}
