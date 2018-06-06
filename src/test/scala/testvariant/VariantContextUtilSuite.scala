package testvariant

import htsjdk.variant.variantcontext.Allele
import org.scalatest.FunSuite
import variant.VariantContextUtil

class VariantContextUtilSuite extends FunSuite {

  test("Samples with allele") {
    // Correctly identifies samples with allele and excludes samples without allele
    assert(VariantContextUtil.samplesWithAllele(vcfRecordRs9628390, Allele.create("A")).toSet
      === Set("HG01048", "HG02594"))
    assert(VariantContextUtil.samplesWithAllele(vcfRecordRs9628390, Allele.create("G")).size === 20)
    // Empty list if there aren't any
    assert(VariantContextUtil.samplesWithAllele(vcfRecordRs9628390, Allele.create("C")).size === 0)
  }

}
