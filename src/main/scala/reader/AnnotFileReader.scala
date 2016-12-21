package reader

import feature.Exceptions.CDSSizeException
import feature.{Block, Feature, FeatureBuilder, GenericFeature, Transcript, MessengerRNA}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.io.Source

/** A reader that loads an annotation file and constructs the corresponding set of [[Feature]]s
  *
  * Implementations include [[GTF22Reader]] for GTF2.2 format.
  */
trait AnnotFileReader {

  /** Returns the set of [[Feature]]s specified in an annotation file.
    *
    * @param file File path
    * @return The set of [[Feature]]s specified in the file. No particular ordering is guaranteed.
    */
  def load(file: String): Set[Feature] = load(Source.fromFile(file).getLines())

  /** Returns the set of [[Feature]]s specified in a sequence of strings. The strings are interpreted
    * as the lines of an annotation file in the order of the iterator.
    *
    * @param lines Iterator over a sequence of lines in an annotation file format
    * @return The set of [[Feature]]s specified in the lines. No particular ordering is guaranteed.
    */
  def load(lines: Iterator[String]): Set[Feature]

}

/** A reader for annotation files in GTF2.2 format ([[http://mblab.wustl.edu/GTF22.html Format specification]])
  *
  * GTF2.2 format uses multiple lines to specify the attributes of a complex feature such as a transcript
  * or messenger RNA. The [[Feature]]s constructed by this reader are the full complex features including
  * multiple exons, a CDS, etc. In other words, the number of [[Feature]]s returned is likely to be much smaller
  * than the number of lines in the GTF2.2 file. Mature RNAs are included as [[Transcript]]s or [[MessengerRNA]]s.
  *
  * Records that do not specify part of a mature RNA (such as CNS types) are included as single [[GenericFeature]]s.
  * These [[GenericFeature]]s are given a name constructed from the location and associated transcript ID if there
  * is one, as with the intron_CNS feature type.
  *
  */
object GTF22Reader extends AnnotFileReader {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Returns the set of [[Feature]]s specified in a sequence of strings representing lines
    * of a file in GTF2.2 format.
    *
    * The number of [[Feature]]s returned is likely to be much smaller than the number of
    * GTF2.2-formatted lines (see object documentation).
    *
    * @param lines Iterator over a sequence of lines in GTF2.2 format
    * @return The set of [[Feature]]s specified in the lines. No particular ordering is guaranteed.
    */
  def load(lines: Iterator[String]): Set[Feature] = {

    // Map of feature ID to FeatureBuilder
    val builders = new scala.collection.mutable.HashMap[String, FeatureBuilder]
    // Set of GenericFeatures that don't need a FeatureBuilder because they have no transcript ID
    val genericFeatures = new mutable.HashSet[Feature]

    lines.foreach(line => {
      // Create a GTF2Record from the line
      val gr = new GTF22Record(line)
      if(!gr.ignore) {
        // Make the block
        val blk = Block(gr.chr, gr.start, gr.end, gr.orientation)
        // Match on feature type
        gr.featureType match {
          case t: MatureRNA =>
            // Only mature RNAs can have multiple GTF records
            val fid = gr.transcriptId.get
            // See if there's already a FeatureBuilder for this transcript ID
            val fb: Option[FeatureBuilder] = builders.get(fid)
            // If no FeatureBuilder for this transcript ID, make a new one with this transcript ID and gene ID
            if (fb.isEmpty) builders.put(fid, {
              val newFb = new FeatureBuilder().setFeatureId(fid)
              // Add the gene ID if there is one
              if (gr.geneId.isDefined) newFb.setGeneId(gr.geneId.get)
              else newFb
            })
            // Replace the FeatureBuilder for this transcript ID with one that has been operated on
            // by this GTF2Record
            builders.put(fid, gr.op(builders(fid)))
          case nm =>
            // Construct a name for the feature
            val sb = new StringBuilder()
              .append(nm.name)
              .append("_")
              .append(gr.chr)
              .append("_")
              .append(gr.start)
              .append("_")
              .append(gr.end)
            // Not part of mature transcript; just make a GenericFeature defined by this record only
            if (gr.transcriptId.isEmpty) genericFeatures.add(new GenericFeature(blk, Some(sb.toString)))
            else
            // Feature has a transcript name but is not part of a mature transcript
            // Make a GenericFeature defined by this record only, and give it a name using the feature type,
            // location, and transcript ID
            genericFeatures.add(new GenericFeature(blk, Some(
              sb.append("_")
              .append(gr.transcriptId.get)
              .toString)))
        }
      } else logger.debug(s"Ignoring line:\t$line")
    })

    // Build the Feature for each FeatureBuilder value in the map, make into a set, then add in
    // all the GenericFeatures
    builders
      .values
      .flatMap(b => {
        try {
          Some(b.get())
        } catch {
          case c: CDSSizeException =>
            // Skip features that throw a CDS size exception
            logger.warn(s"Skipping ${
              if(b.featureId.isDefined) b.featureId.get
              else "feature"
            }. Caught ${c.getClass}: ${c.getMessage}")
            None
          case e: Exception => throw e
        }
      })
      .toSet
      .++(genericFeatures)

  }

}


