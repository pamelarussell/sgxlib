package sequencing

import feature.Feature
import htsjdk.samtools.SamReader

object SamReaderUtils {

  def compatibleRecords(reader: SamReader, feat: Feature): Iterator[SamMapping] = {
    ???
  }

}
