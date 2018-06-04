package testvariant

import variant.VariantContextMutations._

import java.io.File

import org.scalatest.FunSuite

class VariantContextMutationSuite extends FunSuite {

  test("Can read VCF file") {
    assert(vcfFile.canRead)
    assert(vcfFileWithInfo.canRead)
    assert(vcfFileWrongInfo.canRead)
  }

  test("Can get VCF record") {
    assert(vcfRecordTriAllelicRs9306245.getID === "rs9306245")
    assert(vcfRecordTriAllelicRs8143078withInfo.getID === "rs8143078")
    assert(vcfRecordTriAllelicRs8143078wrongInfo.getID === "rs8143078")
  }

  test("Set genotype to missing") {
    assertThrows[IllegalArgumentException] {
      setGenotypeToMissing(vcfRecordTriAllelicRs9306245, "fakeID")
    }
    val gtMissing = setGenotypeToMissing(vcfRecordTriAllelicRs9306245, "HG02660")
    assert(!gtMissing.getGenotype("HG02660").isCalled)
    assert(gtMissing.getGenotype("HG02660").isNoCall)
    assert(gtMissing.getGenotype("HG03571").isCalled)
    assert(gtMissing.getGenotype("HG00638").isHomRef)
  }

  test("Set genotype to missing when there are incorrect info fields") {
    assertThrows[Exception] {setGenotypeToMissing(vcfRecordTriAllelicRs8143078wrongInfo, "HG01048")}
  }

}
