package collection

import feature._
import htsjdk.samtools.util.IntervalTree
import org.slf4j.LoggerFactory
import format.{GTF22Record, MatureRNA}

import scala.collection.immutable.TreeSet
import scala.collection.JavaConversions.asScalaIterator
import scala.collection.mutable
import scala.io.Source

/** A data structure storing a set of [[Feature]]s.
  *
  * Supports common operations on collections of genomic features, such as extracting overlappers
  * of a given locus, and some common collection operations such as contains.
  *
  * @tparam T The type of [[Feature]]s contained in the set
  */
trait FeatureSet[T <: Feature] {

  /** Returns a boolean value representing whether this set contains the given [[Feature]].
    *
    * @param t [[Feature]] to look for
    */
  def contains(t: T): Boolean

  /** The number of features contained in this set. */
  val size: Long

  /** Returns an iterator over overlappers of a genomic interval.
    *
    * Overlappers are the [[Feature]]s contained in this set that overlap the interval
    * accounting for [[Orientation]], as defined by [[Feature.overlaps]].
    *
    * The returned iterator is not guaranteed to conform to any particular ordering.
    *
    * @param chr Chromosome name of query interval
    * @param start Zero-based inclusive start position of query interval
    * @param end Zero-based exclusive end position of query interval
    * @param orientation Orientation of query interval. Only overlappers with a compatible
    *                    [[Orientation]] (as defined by [[Orientation.isCompatible]]) will
    *                    be returned.
    * @return Iterator over overlappers in no particular order, or [[Iterator.empty]] if
    *         there are no overlappers
    */
  def overlappers(chr: String, start: Int, end: Int, orientation: Orientation): Iterator[T]

  /** Returns an iterator over overlappers of a given [[Feature]].
    *
    * Overlappers are the [[Feature]]s contained in this set that overlap the given
    * [[Feature]] as defined by [[Feature.overlaps]].
    *
    * The returned iterator is not guaranteed to conform to any particular ordering.
    *
    * @param feat Query [[Feature]]
    * @return Iterator over overlappers of the given [[Feature]] in no particular order,
    *         or [[Iterator.empty]] if there are no overlappers
    */
  def overlappers(feat: Feature): Iterator[T]

}

/** A [[FeatureSet]] constructed from annotations specified in a GTF2.2 file.
  *
  * GTF2.2 [[http://mblab.wustl.edu/GTF22.html format specification]]. See [[GTF22Record]] for details
  * on how GTF2.2 records are interpreted and incorporated into [[Feature]]s.
  *
  * @param file Path of GTF2.2 file
  */
final class GTF22FeatureSet(file: String) extends FeatureSet[Feature] {

  private val logger = LoggerFactory.getLogger(getClass)

  /*
  Immutable map of chromosome to IntervalTree. Each IntervalTree maps intervals to an immutable TreeSet of features
  with the start and end coordinates of the interval.
   */
  private val tree: Map[String, IntervalTree[TreeSet[Feature]]] = {

    // Enter records into a mutable map for now
    val m: mutable.Map[String, IntervalTree[TreeSet[Feature]]] = new mutable.HashMap[String, IntervalTree[TreeSet[Feature]]]

    // Function to incorporate a new feature into the appropriate IntervalTree
    def addFeat(f: Feature): Unit = {
      // Add the chromosome if it hasn't been seen
      val chr = f.getChr
      if(!m.contains(chr)) m.update(chr, new IntervalTree[TreeSet[Feature]])
      // IntervalTree is not thread-safe
      m(chr).synchronized {
        val st = f.getStart
        val en = f.getEnd
        // IntervalTree uses closed intervals
        val ts = m(chr).find(st, en - 1)
        // Add this feature to the appropriate TreeSet
        if (ts == null) m(chr).put(st, en - 1, TreeSet(f))
        else m(chr).put(st, en - 1, ts.getValue + f)
      }
    }

    // Read the GTF file
    val lines: Iterator[String] = Source.fromFile(file).getLines()
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

    // Get the features from the builders and add them
    builders
      .values
      .flatMap(b => Some(b.get()))
      .foreach(addFeat)

    // Add all the generic features
    genericFeatures.foreach(addFeat)

    // Convert to an immutable map
    m.toMap
  }

  /** Returns a boolean value representing whether this set contains the given [[Feature]].
    *
    * @param t [[Feature]] to look for
    */
  override def contains(t: Feature): Boolean = {
    tree.get(t.getChr) match {
      case None => false
      case Some(it) =>
        val node: IntervalTree.Node[TreeSet[Feature]] = it.find(t.getStart, t.getEnd - 1)
        node match {
          case null => false
          case _ =>
            val ts: TreeSet[Feature] = node.getValue
            ts.contains(t)
        }
    }
  }

  /** The number of features contained in this set. */
  override lazy val size: Long = tree.values.map(it => {
    var cnt: Long = 0
    val iter = it.iterator()
    while(iter.hasNext) {
      val tr = iter.next().getValue
      cnt = cnt + tr.size
    }
    cnt
  }).sum

  /** Returns an iterator over overlappers of a genomic interval.
    *
    * Overlappers are the [[Feature]]s contained in this set that overlap the interval
    * accounting for [[Orientation]], as defined by [[Feature.overlaps]].
    *
    * The returned iterator is not guaranteed to conform to any particular ordering.
    *
    * @param chr Chromosome name of query interval
    * @param start Zero-based inclusive start position of query interval
    * @param end Zero-based exclusive end position of query interval
    * @param orientation Orientation of query interval. Only overlappers with a compatible
    *                    [[Orientation]] (as defined by [[Orientation.isCompatible]]) will
    *                    be returned.
    * @return Iterator over overlappers in no particular order, or [[Iterator.empty]] if
    *         there are no overlappers
    */
  override def overlappers(chr: String, start: Int, end: Int, orientation: Orientation): Iterator[Feature] =
    if(tree.contains(chr)) overlappers(new GenericFeature(Block(chr, start, end, orientation), None))
    else {
      val c = chr.replaceFirst("^chr", "")
      if(tree.contains(c)) overlappers(new GenericFeature(Block(c, start, end, orientation), None))
      else Iterator.empty
    }

  /** Returns an iterator over overlappers of a given [[Feature]].
    *
    * Overlappers are the [[Feature]]s contained in this set that overlap the given
    * [[Feature]] as defined by [[Feature.overlaps]].
    *
    * The returned iterator is not guaranteed to conform to any particular ordering.
    *
    * @param feat Query [[Feature]]
    * @return Iterator over overlappers of the given [[Feature]] in no particular order,
    *         or [[Iterator.empty]] if there are no overlappers
    */
  override def overlappers(feat: Feature): Iterator[Feature] = {
    tree.get(feat.getChr) match {
      case None => Iterator.empty
      case Some(it) =>
        asScalaIterator(it.overlappers(feat.getStart, feat.getEnd - 1))
          .map(node => node.getValue)
          .flatMap(ts => ts.iterator.filter(f => feat.overlaps(f)))
    }
  }

}



