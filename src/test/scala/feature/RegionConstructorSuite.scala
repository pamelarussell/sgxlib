package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class RegionConstructorSuite extends FunSuite {

  constructorBlock()
  constructorBlockSet()
  illegalBlock()
  illegalBlockSets()

  def constructorBlock(): Unit = {
    test("Block constructor") {
      intercept[IllegalArgumentException](Block("", -1, 20, Plus))
      intercept[IllegalArgumentException](Block("", 10, 20, Plus))
      intercept[IllegalArgumentException](Block("chr1", 20, 10, Plus))
    }
  }

  def constructorBlockSet(): Unit = {
    test("BlockSet constructor") {
      // Adjacent blocks same orientation
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr1_2000_3000_plus)))
      // Non-overlapping blocks different orientation
      intercept[IllegalArgumentException](BlockSet(List(chr2_1000_2000_plus, chr2_3000_4000_both)))
      // Overlapping blocks
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr1_1500_2500_plus)))
      // Empty block list
      intercept[IllegalArgumentException](BlockSet(Nil))
      // Blocks on different chromosomes
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr2_3000_4000_plus)))
      // Blocks in wrong order
      intercept[IllegalArgumentException](BlockSet(List(chr2_3000_4000_plus, chr2_1000_2000_plus)))
    }
  }

  def illegalBlock(): Unit = {
    test("Illegal blocks") {
      intercept[IllegalArgumentException](Block("", 100, 200, Plus))
      intercept[IllegalArgumentException](Block("chr1", 100, 100, Plus))
      intercept[IllegalArgumentException](Block("chr1", 100, 99, Plus))
    }
  }

  def illegalBlockSets(): Unit = {
    test("Illegal BlockSets") {
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_minus))) // One block
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_minus, chr1_5_6_minus))) // Blocks in wrong order
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr1_1500_2500_plus))) // Blocks overlap
      intercept[IllegalArgumentException](BlockSet(List(chr1_5_6_minus, chr1_6_7_minus))) // Blocks are touching
      intercept[IllegalArgumentException](BlockSet(List(chr1_5_6_minus, chr1_1000_2000_plus_1))) // Different orientations
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr2_3000_4000_plus))) // Different chromosomes
    }
  }


}
