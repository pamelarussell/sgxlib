package feature

final class FeatureBuilder {

  private val blocks: scala.collection.mutable.TreeSet[Region] = new scala.collection.mutable.TreeSet[Region]()
  private var cdsStart: Option[Int] = None
  private var cdsEnd: Option[Int] = None
  private var featureId: Option[String] = None
  private var geneId: Option[String] = None

  def addBlock(b: Block): Unit = blocks.add(b)
  def setCdsStart(s: Int): Unit = cdsStart = Some(s)
  def setCdsEnd(e: Int): Unit = cdsEnd = Some(e)
  def setFeatureId(t: String): Unit = featureId = Some(t)
  def setGeneId(g: String): Unit = geneId = Some(g)

  private final case class Data(blocks: List[Block],
                                cdsStart: Option[Int],
                                cdsEnd: Option[Int],
                                featureId: Option[String],
                                geneId: Option[String])

  def get(): Feature = {

    // Convert mutable treeset of regions to immutable list of blocks
    def regionsToBlocks(bs: scala.collection.mutable.TreeSet[Region]): List[Block] = {
      blocks.toList.map {
        case b: Block => b
        case _ => throw new IllegalArgumentException("Not a Block")
      }
    }

    val data = Data(regionsToBlocks(blocks), cdsStart, cdsEnd, featureId, geneId)

    data match {
      // mRNA with one block
      case Data(blk :: Nil, Some(cs), Some(ce), f, g) => MessengerRNA(blk, cs, ce, f, g)
      // mRNA with multiple blocks
      case Data(blk1 :: blk2 :: tail, Some(cs), Some(ce), f, g) => MessengerRNA(BlockSet(data.blocks), cs, ce, f, g)
      // Transcript with one block
      case Data(blk :: Nil, None, None, f, Some(g)) => new Transcript(blk, f, Some(g))
      // Transcript with multiple blocks
      case Data(blk1 :: blk2 :: tail, None, None, f, Some(g)) => new Transcript(BlockSet(data.blocks), f, Some(g))
      // Generic block
      case Data(blk :: Nil, None, None, f, None) => new GenericFeature(blk, f)
      // Generic block set
      case Data(blk1 :: blk2 :: tail, None, None, f, None) => new GenericFeature(BlockSet(data.blocks), f)
      // None of the above
      case _ => throw new IllegalArgumentException(s"Invalid feature builder:\n" +
        s"Blocks:\t${data.blocks.toString()}\n" +
        s"CDS start:\t${data.cdsStart.toString}\n" +
        s"CDS end:\t${data.cdsEnd.toString}\n" +
        s"Transcript ID:\t${data.featureId.toString}\n" +
        s"Gene ID:\t${data.geneId.toString}\n")
    }

  }

}
