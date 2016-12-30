package feature

import feature.Exceptions.CDSSizeException
import org.slf4j.LoggerFactory


/** A builder for [[Feature]]s.
  *
  * [[Feature]] properties such as [[Block]]s, name, gene name, and CDS start and end can be added one by one.
  * The appropriate type in the [[Feature]] hierarchy is determined automatically based on the properties that have been added.
  *
  * The [[Feature]] is obtained by calling [[get]].
  *
  * The following are the valid combinations of provided properties and the types they give rise to.
  *
  * One or more blocks, optional feature name, no gene name, no CDS start, no CDS end -> [[GenericFeature]]
  *
  * One or more blocks, optional feature name, a gene name, no CDS start, no CDS end -> [[Transcript]]
  *
  * One or more blocks, optional feature name, optional gene name, a CDS start, a CDS end -> [[MessengerRNA]]. If the CDS is invalid
  * (length < 6 or length not divisible by 3) a [[Transcript]] is returned instead.
  *
  * Any other combination of properties results in an IllegalArgumentException when calling [[get]].
  *
  */
final class FeatureBuilder(val blocks: List[Block] = Nil,
                           val cdsStart: Option[Int] = None,
                           val cdsEnd: Option[Int] = None,
                           val featureId: Option[String] = None,
                           val geneId: Option[String] = None) {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Returns a new FeatureBuilder with an additional [[Block]] added.
    *
    * The new [[Block]] must be on the same chromosome as any existing [[Block]]s in this [[FeatureBuilder]],
    * have the same [[Orientation]], and not overlap any existing [[Block]]s.
    *
    * @param b New [[Block]] to add
    * @return New [[FeatureBuilder]] with the same properties as this plus the new [[Block]] added
    */
  def addBlock(b: Block): FeatureBuilder = {
    new FeatureBuilder(blocks = b :: blocks,
      cdsStart = this.cdsStart,
      cdsEnd = this.cdsEnd,
      featureId = this.featureId,
      geneId = this.geneId)
  }

  /** Returns a new [[FeatureBuilder]] with the specified CDS start.
    *
    * If this [[FeatureBuilder]] already has a CDS start specified, the returned object will have the
    * new CDS start and will have no memory of the previous CDS start.
    *
    * @param s New zero-based CDS start position as in [[MessengerRNA.cdsStart]]
    * @return New [[FeatureBuilder]] with the same properties as this but the new CDS start
    */
  def setCdsStart(s: Int): FeatureBuilder = {
    new FeatureBuilder(blocks = this.blocks,
      cdsStart = Some(s),
      cdsEnd = this.cdsEnd,
      featureId = this.featureId,
      geneId = this.geneId)
  }

  /** Returns a new [[FeatureBuilder]] with the specified CDS end.
    *
    * If this [[FeatureBuilder]] already has a CDS end specified, the returned object will have the
    * new CDS end and will have no memory of the previous CDS end.
    *
    * @param e New zero-based CDS end position as in [[MessengerRNA.cdsEnd]]
    * @return New [[FeatureBuilder]] with the same properties as this but the new CDS end
    */
  def setCdsEnd(e: Int): FeatureBuilder = {
    new FeatureBuilder(blocks = this.blocks,
      cdsStart = this.cdsStart,
      cdsEnd = Some(e),
      featureId = this.featureId,
      geneId = this.geneId)
  }

  /** Returns a new [[FeatureBuilder]] with the specified feature name.
    *
    * If this [[FeatureBuilder]] already has a feature name specified, the returned object will have the
    * new feature name and will have no memory of the previous feature name.
    *
    * @param t New feature name
    * @return New [[FeatureBuilder]] with the same properties as this but the new feature name
    */
  def setFeatureId(t: String): FeatureBuilder = {
    new FeatureBuilder(blocks = this.blocks,
      cdsStart = this.cdsStart,
      cdsEnd = this.cdsEnd,
      featureId = Some(t),
      geneId = this.geneId)
  }

  /** Returns a new [[FeatureBuilder]] with the specified gene name.
    *
    * If this [[FeatureBuilder]] already has a gene name specified, the returned object will have the
    * new gene name and will have no memory of the previous gene name.
    *
    * @param g New gene name
    * @return New [[FeatureBuilder]] with the same properties as this but the new gene name
    */
  def setGeneId(g: String): FeatureBuilder = {
    new FeatureBuilder(blocks = this.blocks,
      cdsStart = this.cdsStart,
      cdsEnd = this.cdsEnd,
      featureId = this.featureId,
      geneId = Some(g))
  }

  /** Returns a [[Feature]] built from the provided properties.
    *
    * The type of the returned object depends on which properties have been provided.
    * Certain combinations of provided properties are invalid.
    * See [[FeatureBuilder]] documentation for details.
    *
    * @return The specified [[Feature]]
    */
  def get(): Feature = {

    // Make a region by combining the blocks. Overlapping blocks are merged.
    val region = blocks.foldLeft[Region](Empty)((r, b) => r.addBlock(b))

    (cdsStart, cdsEnd, featureId, geneId) match {
      // mRNA
      case (Some(cs), Some(ce), f, g) =>
        // Try to construct a messenger RNA
        try {
          MessengerRNA(region, cs, ce, f, g)
        } catch {
          // If CDS size is invalid, return a transcript instead
          case c: CDSSizeException => new Transcript(region, f, g)
          case e: Exception => throw e
        }
      // Transcript
      case (None, None, f, Some(g)) => new Transcript(region, f, Some(g))
      // Generic feature
      case (None, None, f, None) => new GenericFeature(region, f)
      // None of the above
      case _ => throw new IllegalArgumentException(s"Invalid feature builder:\n" +
        s"Blocks:\t${region.toString}\n" +
        s"CDS start:\t${cdsStart.toString}\n" +
        s"CDS end:\t${cdsEnd.toString}\n" +
        s"Transcript ID:\t${featureId.toString}\n" +
        s"Gene ID:\t${geneId.toString}\n")
    }

  }

}

/**
  * A way to encapsulate arbitrary operations on a [[FeatureBuilder]].
  *
  * The [[op]] function performs the operations on a specified [[FeatureBuilder]]
  * and returns a new modified [[FeatureBuilder]].
  *
  * Classes that mix in this trait include [[format.GTF22Record]].
  *
  */
trait FeatureBuilderModifier {

  /** Returns a new, possibly modified [[FeatureBuilder]].
    *
    * @param fb Original [[FeatureBuilder]]
    * @return New [[FeatureBuilder]] resulting from applying the function to the original
    */
  def op(fb: FeatureBuilder): FeatureBuilder

}