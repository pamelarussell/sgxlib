package collection

import feature.{Block, Feature, GenericFeature, Orientation}
import htsjdk.samtools.util.IntervalTree
import reader.GTF22Reader

import scala.collection.immutable.TreeSet
import scala.collection.JavaConversions.asScalaIterator
import scala.collection.mutable

trait FeatureSet[T <: Feature] {

  def contains(t: T): Boolean

  val size: Long

  def overlappers(chr: String, start: Int, end: Int, orientation: Orientation): Iterator[T]

  def overlappers(feat: Feature): Iterator[T]

}

final class GTF22FeatureSet(file: String) extends FeatureSet[Feature] {

  private val tree: Map[String, IntervalTree[TreeSet[Feature]]] = {
    val m: mutable.Map[String, IntervalTree[TreeSet[Feature]]] = new mutable.HashMap[String, IntervalTree[TreeSet[Feature]]]
    GTF22Reader.load(file).foreach(f => {
      val chr = f.getChr
      if(!m.contains(chr)) m.update(chr, new IntervalTree[TreeSet[Feature]])
      m(chr).synchronized {
        val st = f.getStart
        val en = f.getEnd
        val ts = m(chr).find(st, en - 1)
        if (ts == null) m(chr).put(st, en - 1, TreeSet(f))
        else m(chr).put(st, en - 1, ts.getValue + f)
      }
    })
    m.toMap
  }

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

  override lazy val size: Long = tree.values.map(it => {
    var cnt: Long = 0
    val iter = it.iterator()
    while(iter.hasNext) {
      val tr = iter.next().getValue
      cnt = cnt + tr.size
    }
    cnt
  }).sum

  override def overlappers(chr: String, start: Int, end: Int, orientation: Orientation): Iterator[Feature] =
    overlappers(new GenericFeature(Block(chr, start, end, orientation), None))

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



