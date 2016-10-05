package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 8/30/16.
  */
class FeatureMiscSuite extends FunSuite {

  // Run all tests
  adjacencyBlockBlock()
  featureEmptiness()
  featureChr()
  featureStart()
  featureEnd()
  featureOrientation()
  featureName()
  featureGetBlocks()
  featureRename()


  def featureNumBlocks(): Unit = {
    test("Number of blocks") {
      assert(Empty.numBlocks === 0, "Empty feature should have 0 blocks")
      assert(chr1_5_6_minus.numBlocks === 1, "Block should have 1 block")
      assert(chr1_500_600_700_800_900_1100_both.numBlocks === 3, "Block set with 3 blocks")
    }
  }


  def featureEmptiness(): Unit = {
    test("Feature emptiness") {
      assert(Empty.isEmpty, "Empty feature should be empty")
      assert(!chr1_1000_2000_plus_1.isEmpty, "Nonempty block should not be empty")
      assert(!chr1_100_200_300_400_plus.isEmpty, "Block set should not be empty")
    }
  }


  def adjacencyBlockBlock(): Unit = {
    test("Block adjacency with block") {
      assert(Block("chr1", 1, 5, Plus, "name").adjacent(Block("chr1", 5, 10, Minus, "name")))
      assert(!Block("chr1", 1, 5, Plus, "name").adjacent(Block("chr1", 6, 10, Plus, "name")))
      assert(!Block("chr1", 1, 5, Plus, "name").adjacent(Block("chr1", 4, 10, Plus, "name")))
      assert(!Block("chr1", 1, 5, Plus, "name").adjacent(Block("chr2", 5, 10, Plus, "name")))
      assert(Block("chr1", 10, 12, Plus, "name").adjacent(Block("chr1", 6, 10, Minus, "name")))
      assert(Block("chr1", 10, 11, Plus, "name").adjacent(Block("chr1", 9, 10, Minus, "name")))
    }
  }


  def featureChr(): Unit = {
    test("Feature chr") {
      assert(chr1_1000_2000_both.chr === "chr1")
      assert(chr1_100_200_300_400_plus.chr === "chr1")
      intercept[IllegalStateException](Empty.chr)
    }
  }

  def featureStart(): Unit = {
    test("Feature start") {
      assert(chr1_1000_2000_both.start === 1000)
      assert(chr1_100_200_300_400_plus.start === 100)
      intercept[IllegalStateException](Empty.start)
    }
  }

  def featureEnd(): Unit = {
    test("Feature end") {
      assert(chr2_1000_2000_plus.end === 2000)
      assert(chr1_100_200_300_400_plus.end === 400)
      intercept[IllegalStateException](Empty.end)
    }
  }

  def featureOrientation(): Unit = {
    test("Feature orientation") {
      assert(chr2_1000_2000_plus.orientation === Plus)
      assert(chr1_1000_2000_unstranded.orientation === Unstranded)
      assert(chr1_100_200_300_400_plus.orientation === Plus)
      intercept[IllegalStateException](Empty.orientation)
    }
  }

  def featureName(): Unit = {
    test("Feature name") {
      assert(chr1_1000_2000_minus.name === "block1")
      assert(chr1_100_200_300_400_plus.name === "bs1")
      intercept[IllegalStateException](Empty.name)
    }
  }

  def featureGetBlocks(): Unit = {
    test("Feature get blocks") {
      assert(chr1_1000_2000_minus.blocks === List(Block("chr1", 1000, 2000, Minus, chr1_1000_2000_minus.name)))
      assert(chr1_100_200_300_400_plus.blocks === List(
        Block("chr1", 100, 200, Plus, "block0"),
        Block("chr1", 300, 400, Plus, "block1")
      ))
      assert(Empty.blocks === Nil)
    }
  }

  def featureRename(): Unit = {
    test("Feature rename") {
      assert(chr2_3000_4000_plus.rename("newname") === Block("chr2", 3000, 4000, Plus, "newname"))
      assert(chr1_100_200_300_400_plus.rename("newname") === BlockSet(List(
        Block("chr1", 100, 200, Plus, "block0"),
        Block("chr1", 300, 400, Plus, "block1")
      ), "newname"))
      intercept[IllegalStateException](Empty.rename("name"))
    }
  }


}
