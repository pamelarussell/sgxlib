package testsequencing

import feature._
import org.scalatest.FunSuite
import sequencing.SamMapping

/**
  * Created by prussell on 2/6/17.
  */
class SamMappingSuite extends FunSuite {

  test("Unmapped record") {
    intercept[IllegalArgumentException](SamMapping(unmapped_paired, Plus))
    intercept[IllegalArgumentException](SamMapping(unmapped_unpaired, Plus))
  }

  test("SamMapping blocks and name - reads paired") {
    val smp: SamMapping = SamMapping(DRR0237527248016_paired, Plus)
    assert(smp.blocks === BlockSet(List(
      Block("20", 37203934, 37203960, Minus), Block("20", 37204766, 37204779, Minus))))
    assert(smp.name === Some("DRR023752.7248016"))
    val smu: SamMapping = SamMapping(DRR0237527248016_paired, Unstranded)
    assert(smu.blocks === BlockSet(List(
      Block("20", 37203934, 37203960, Unstranded), Block("20", 37204766, 37204779, Unstranded))))
    assert(smu.name === Some("DRR023752.7248016"))
    val sm3: SamMapping = SamMapping(DRR02375216305998_paired, Unstranded)
    assert(sm3.blocks === Block("20", 37003764, 37003803, Unstranded))
    assert(sm3.name === Some("DRR023752.16305998"))
  }

  test("SamMapping blocks and name - reads unpaired") {
    val smp: SamMapping = SamMapping(DRR02375217157817_unpaired, Plus)
    assert(smp.blocks === Block("20", 37521350, 37521389, Minus))
    assert(smp.name === Some("DRR023752.17157817"))
    val smu: SamMapping = SamMapping(DRR02375217157817_unpaired, Unstranded)
    assert(smu.blocks === Block("20", 37521350, 37521389, Unstranded))
    assert(smu.name === Some("DRR023752.17157817"))
  }

}
