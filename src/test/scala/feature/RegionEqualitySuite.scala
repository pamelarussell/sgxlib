package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 9/30/16.
  */
class RegionEqualitySuite extends FunSuite {

  equalityBlock()
  equalityBlockSet()
  equalityEmpty()

  def equalityEmpty(): Unit = {
    test("Empty equality") {
      assert(Empty === Empty, "Empty should be equal to Empty")
    }
  }

  def equalityBlock(): Unit = {
    test("Block equality") {
      assert(chr1_1000_2000_plus_1 === chr1_1000_2000_plus_3, "Blocks with same span and same name should be equal")
      assert(!(Block("chr1", 1000, 2000, Plus) === Block("chr2", 1000, 2000, Plus)), "Blocks with different chromosomes should not be equal")
      assert(!(Block("chr1", 1000, 2000, Plus) === Block("chr1", 1000, 2000, Both)), "Blocks with different strand should not be equal")
    }
  }

  def equalityBlockSet(): Unit = {
    test("BlockSet equality") {
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus),
        Block("chr1", 300, 400, Plus)
      )) === BlockSet(List(
        Block("chr1", 100, 200, Plus),
        Block("chr1", 300, 400, Plus)
      )), "Block set equality")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus),
        Block("chr1", 300, 400, Plus)
      )) != BlockSet(List(
        Block("chr2", 100, 200, Plus),
        Block("chr2", 300, 400, Plus)
      )), "Block sets with different chromosomes should not be equal")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus),
        Block("chr1", 300, 400, Plus)
      )) != BlockSet(List(
        Block("chr1", 100, 200, Both),
        Block("chr1", 300, 400, Both)
      )), "Block sets with different strands should not be equal")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus),
        Block("chr1", 300, 400, Plus)
      )) != BlockSet(List(
        Block("chr1", 100, 200, Plus),
        Block("chr1", 300, 401, Plus)
      )), "Block sets with different blocks should not be equal")
      assert(BlockSet(List(
        Block("chr1", 100, 200, Plus),
        Block("chr1", 300, 400, Plus)
      )) != BlockSet(List(
        Block("chr1", 100, 200, Plus),
        Block("chr1", 300, 400, Plus),
        Block("chr1", 500, 600, Plus)
      )), "Block sets with different blocks should not be equal")
    }
  }



}