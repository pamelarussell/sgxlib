package testvariant

import variant.VariantContextMutations._
import java.io.File

import htsjdk.variant.variantcontext.Allele
import org.scalatest.FunSuite
import variant.VariantContextMutations

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

  test("Remove allele") {
    // Can't remove an allele that doesn't exist
    assertThrows[Exception](VariantContextMutations.removeAllele(vcfRecordRs9628390, Allele.create("C")))
    // Remove an allele that does exist; check correct samples are altered and allele list is altered
    val vc = VariantContextMutations.removeAllele(vcfRecordTriAllelicRs9306245, Allele.create("A"))
    assert(vc.getGenotype("HG01048").isNoCall)
    assert(vc.getGenotype("HG00638").isHomRef)
    assert(vc.getGenotype("HG02570").isHet)
    assert(vc.getGenotype("HG01586").isHomVar)
    assert(!vc.hasAllele(Allele.create("A")))
    assert(vc.hasAllele(Allele.create("C"), true))
    assert(vc.hasAllele(Allele.create("T")))
    // Can't remove the reference allele
    assertThrows[IllegalArgumentException](VariantContextMutations.removeAllele(vcfRecordTriAllelicRs9306245, Allele.create("C")))
  }

}
