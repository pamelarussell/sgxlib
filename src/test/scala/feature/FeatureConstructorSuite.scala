package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class FeatureConstructorSuite extends FunSuite {

  constructorBlock()
  constructorBlockSet()
  illegalBlock()
  illegalBlockSets()

  def constructorBlock(): Unit = {
    test("Block constructor") {
      intercept[IllegalArgumentException](Block("", -1, 20, Plus, "name"))
      intercept[IllegalArgumentException](Block("", 10, 20, Plus, "name"))
      intercept[IllegalArgumentException](Block("chr1", 20, 10, Plus, "name"))
    }
  }

  def constructorBlockSet(): Unit = {
    test("BlockSet constructor") {
      // Adjacent blocks same orientation
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr1_2000_3000_plus), "name"))
      // Non-overlapping blocks different orientation
      intercept[IllegalArgumentException](BlockSet(List(chr2_1000_2000_plus, chr2_3000_4000_both), "name"))
      // Overlapping blocks
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr1_1500_2500_plus), "name"))
      // Empty block list
      intercept[IllegalArgumentException](BlockSet(Nil, "name"))
      // Blocks on different chromosomes
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr2_3000_4000_plus), "name"))
      // Blocks in wrong order
      intercept[IllegalArgumentException](BlockSet(List(chr2_3000_4000_plus, chr2_1000_2000_plus), "name"))
    }
  }

  def illegalBlock(): Unit = {
    test("Illegal blocks") {
      intercept[IllegalArgumentException](Block("", 100, 200, Plus, "name"))
      intercept[IllegalArgumentException](Block("chr1", 100, 100, Plus, "name"))
      intercept[IllegalArgumentException](Block("chr1", 100, 99, Plus, "name"))
    }
  }

  def illegalBlockSets(): Unit = {
    test("Illegal BlockSets") {
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_minus), "name")) // One block
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_minus, chr1_5_6_minus), "name")) // Blocks in wrong order
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr1_1500_2500_plus), "name")) // Blocks overlap
      intercept[IllegalArgumentException](BlockSet(List(chr1_5_6_minus, chr1_6_7_minus), "name")) // Blocks are touching
      intercept[IllegalArgumentException](BlockSet(List(chr1_5_6_minus, chr1_1000_2000_plus_1), "name")) // Different orientations
      intercept[IllegalArgumentException](BlockSet(List(chr1_1000_2000_plus_1, chr2_3000_4000_plus), "name")) // Different chromosomes
    }
  }


}
