package feature

import scala.collection.immutable.TreeSet


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
  * One or more blocks, optional feature name, optional gene name, a CDS start, a CDS end -> [[MessengerRNA]]
  *
  * Any other combination of properties results in an IllegalArgumentException when calling [[get]].
  *
  */
final class FeatureBuilder(private val blocks: TreeSet[Block] = new TreeSet[Block],
                           private val cdsStart: Option[Int] = None,
                           private val cdsEnd: Option[Int] = None,
                           private val featureId: Option[String] = None,
                           private val geneId: Option[String] = None) {

  /** Returns a new FeatureBuilder with an additional [[Block]] added.
    *
    * The new [[Block]] must be on the same chromosome as any existing [[Block]]s in this [[FeatureBuilder]],
    * have the same [[Orientation]], and not overlap any existing [[Block]]s.
    *
    * @param b New [[Block]] to add
    * @return New [[FeatureBuilder]] with the same properties as this plus the new [[Block]] added
    */
  def addBlock(b: Block): FeatureBuilder = {
    new FeatureBuilder(blocks = this.blocks + b,
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

    val blkList = blocks.toList

    (blkList, cdsStart, cdsEnd, featureId, geneId) match {
      // mRNA with one block
      case (blk :: Nil, Some(cs), Some(ce), f, g) => MessengerRNA(blk, cs, ce, f, g)
      // mRNA with multiple blocks
      case (blk1 :: blk2 :: tail, Some(cs), Some(ce), f, g) => MessengerRNA(BlockSet(blkList), cs, ce, f, g)
      // Transcript with one block
      case (blk :: Nil, None, None, f, Some(g)) => new Transcript(blk, f, Some(g))
      // Transcript with multiple blocks
      case (blk1 :: blk2 :: tail, None, None, f, Some(g)) => new Transcript(BlockSet(blkList), f, Some(g))
      // Generic block
      case (blk :: Nil, None, None, f, None) => new GenericFeature(blk, f)
      // Generic block set
      case (blk1 :: blk2 :: tail, None, None, f, None) => new GenericFeature(BlockSet(blkList), f)
      // None of the above
      case _ => throw new IllegalArgumentException(s"Invalid feature builder:\n" +
        s"Blocks:\t${blkList.toString()}\n" +
        s"CDS start:\t${cdsStart.toString}\n" +
        s"CDS end:\t${cdsEnd.toString}\n" +
        s"Transcript ID:\t${featureId.toString}\n" +
        s"Gene ID:\t${geneId.toString}\n")
    }

  }

}
