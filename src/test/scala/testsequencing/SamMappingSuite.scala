package testsequencing

import feature._
import org.scalatest.FunSuite
import sequencing.SamMapping

/**
  * Created by prussell on 2/6/17.
  */
class SamMappingSuite extends FunSuite{

  test("SamMapping blocks and name") {
    val smp: SamMapping = SamMapping(DRR0237527248016, Plus)
    assert(smp.blocks === BlockSet(List(
      Block("20", 37203934, 37203960, Minus), Block("20", 37204766, 37204779, Minus))))
    assert(smp.name === Some("DRR023752.7248016"))
    val smu: SamMapping = SamMapping(DRR0237527248016, Unstranded)
    assert(smu.blocks === BlockSet(List(
      Block("20", 37203934, 37203960, Unstranded), Block("20", 37204766, 37204779, Unstranded))))
    assert(smu.name === Some("DRR023752.7248016"))
  }

}
