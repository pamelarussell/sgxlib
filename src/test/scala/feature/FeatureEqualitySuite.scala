package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class FeatureEqualitySuite extends FunSuite {

  equalityBlock()
  equalityBlockSet()
  equalityEmpty()
  featureEqualsIgnoreName()

  def equalityEmpty(): Unit = {
    test("Empty equality") {
      assert(Empty === Empty, "Empty should be equal to Empty")
    }
  }

  def equalityBlock(): Unit = {
    test("Block equality") {
      assert(chr1_1000_2000_plus_1 === chr1_1000_2000_plus_3, "Blocks with same span and same name should be equal")
      assert(!(chr1_1000_2000_plus_1 == chr1_1000_2000_plus_2), "Blocks with different names should not be equal")
      assert(!(Block("chr1", 1000, 2000, Plus, "name") === Block("chr2", 1000, 2000, Plus, "name")), "Blocks with different chromosomes should not be equal")
      assert(!(Block("chr1", 1000, 2000, Plus, "name") === Block("chr1", 1000, 2000, Both, "name")), "Blocks with different strand should not be equal")
    }
  }

  def equalityBlockSet(): Unit = {
    test("BlockSet equality") {
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1")
      ), "name") === BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1")
      ), "name"), "Block set equality")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1")
      ), "name") != BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1")
      ), "name2"), "Block sets with different names should not be equal")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1")
      ), "name") != BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b2")
      ), "name"), "Block sets with different names for blocks should not be equal")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1")
      ), "name") != BlockSet(List(
        Block("chr2", 100, 200, Plus, "b0"),
        Block("chr2", 300, 400, Plus, "b1")
      ), "name"), "Block sets with different chromosomes should not be equal")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1")
      ), "name") != BlockSet(List(
        Block("chr1", 100, 200, Both, "b0"),
        Block("chr1", 300, 400, Both, "b1")
      ), "name"), "Block sets with different strands should not be equal")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1")
      ), "name") != BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 401, Plus, "b1")
      ), "name"), "Block sets with different blocks should not be equal")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1")
      ), "name") != BlockSet(List(
        Block("chr1", 100, 200, Plus, "b0"),
        Block("chr1", 300, 400, Plus, "b1"),
        Block("chr1", 500, 600, Plus, "b1")
      ), "name"), "Block sets with different blocks should not be equal")
    }
  }

  def featureEqualsIgnoreName(): Unit = {
    test("Feature equalsIgnoreName") {
      assert(chr1_1000_2000_plus_1.equalsIgnoreName(chr1_1000_2000_plus_2))
      assert(!chr2_1000_2000_plus.equalsIgnoreName(chr2_3000_4000_plus.rename(chr2_1000_2000_plus.name)))
    }
  }


}
