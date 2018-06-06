import java.io.File

import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.VCFFileReader

package object testvariant {

  val vcfFile: File = new File(getClass.getResource("/chr22_1kg_rand_20_indv_hand_picked_snps.vcf").getPath)
  val vcfFileWithInfo: File = new File(getClass.getResource("/chr22_1kg_rand_20_indv_hand_picked_snps_with_info.vcf").getPath)
  val vcfFileWrongInfo: File = new File(getClass.getResource("/chr22_1kg_rand_20_indv_hand_picked_snps_with_wrong_info.vcf").getPath)
  val vcfReader: VCFFileReader = new VCFFileReader(vcfFile)
  val vcfReaderWithInfo: VCFFileReader = new VCFFileReader(vcfFileWithInfo)
  val vcfReaderWrongInfo: VCFFileReader = new VCFFileReader(vcfFileWrongInfo)

  // Get VariantContext for a particular rs ID
  def getSnp(id: String, vcfReader: VCFFileReader): VariantContext = {
    val matches: Iterable[VariantContext] =
      scala.collection.JavaConverters.asScalaIterator(vcfReader.iterator()).
        filter(vc => vc.getID.equals(id)).
        toSet
    if(matches.size != 1) {
      throw new IllegalArgumentException(s"There are ${matches.size} matches for ID $id")
    } else matches.head
  }

  val vcfRecordRs9628390: VariantContext = getSnp("rs9628390", vcfReader)
  val vcfRecordTriAllelicRs9306245: VariantContext = getSnp("rs9306245", vcfReader)
  val vcfRecordTriAllelicRs8143078withInfo: VariantContext = getSnp("rs8143078", vcfReaderWithInfo)
  val vcfRecordTriAllelicRs8143078wrongInfo: VariantContext = getSnp("rs8143078", vcfReaderWrongInfo)

}
