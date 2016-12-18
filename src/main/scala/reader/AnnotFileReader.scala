package reader

import feature.Exceptions.CDSSizeException
import feature.{Block, Feature, FeatureBuilder, GenericFeature}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.io.Source

trait AnnotFileReader {def load(file: String): Set[Feature]}

object GTF2Reader extends AnnotFileReader {

  private val logger = LoggerFactory.getLogger(getClass)

  def load(lines: Iterator[String]): Set[Feature] = {

    // Map of feature ID to FeatureBuilder
    val builders = new scala.collection.mutable.HashMap[String, FeatureBuilder]
    // Set of GenericFeatures that don't need a FeatureBuilder because they have no transcript ID
    val genericFeatures = new mutable.HashSet[Feature]

    lines.foreach(line => {
      // Create a GTF2Record from the line
      val gr = new GTF2Record(line)
      if(!gr.ignore) {
        // Get the transcript ID
        val fid = gr.transcriptId
        // Make the block
        val blk = Block(gr.chr, gr.start, gr.end, gr.orientation)
        // If no transcript ID, just make a GenericFeature defined by this record only
        if (fid.isEmpty) genericFeatures.add(new GenericFeature(blk, None))
        else {
          // See if there's already a FeatureBuilder for this transcript ID
          val fb: Option[FeatureBuilder] = builders.get(fid.get)
          // If no FeatureBuilder for this transcript ID, make a new one with this transcript ID and gene ID
          if (fb.isEmpty) builders.put(fid.get, {
            val newFb = new FeatureBuilder().setFeatureId(fid.get)
            // Add the gene ID if there is one
            if (gr.geneId.isDefined) newFb.setGeneId(gr.geneId.get)
            else newFb
          })
          // Replace the FeatureBuilder for this transcript ID with one that has been operated on
          // by this GTF2Record
          builders.put(fid.get, gr.op(builders(fid.get)))
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

  override def load(file: String): Set[Feature] = load(Source.fromFile(file).getLines())

}


