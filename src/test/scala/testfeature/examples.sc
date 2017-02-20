import feature._

// Construct two regions
val region1: Region = BlockSet(List(
  Block("1", 1000, 2000, Plus),
  Block("1", 3000, 4000, Plus),
  Block("1", 5000, 6000, Plus),
  Block("1", 7000, 8000, Plus)
))
val region2: Region = BlockSet(List(
  Block("1", 1100, 1200, Plus),
  Block("1", 1300, 1400, Plus),
  Block("1", 3500, 3600, Plus)
))

// Region arithmetic
val contains: Boolean = region1.contains(region2) // true
val intersect: Region = region1.intersection(region2) // [[1:1100-1200:+], [1:1300-1400:+], [1:3500-3600:+]]
val trim: Region = region1.trim(1500, 3500) // [[1:1500-2000:+], [1:3000-3500:+]]

// Convert between genomic position and relative position with respect to region
val relpos: Option[Int] = region1.relativePos(7600) // Some(3600)
val genpos: Int = region1.chrPos(400) // 1400

// Construct a messenger RNA
val mrna: MessengerRNA = MessengerRNA(region1, 3500, 5502, Some("region1"), Some("gene1"))

// Get mRNA sub-features
val cds: Region = mrna.getCDS // [[1:3500-4000:+], [1:5000-5502:+]]
val stopCodon: Region = mrna.getStopCodon // [1:5499-5502:+]
val utr5: Option[Region] = mrna.get5UTR // Some([[1:1000-2000:+], [1:3000-3500:+]])
val introns: List[Block] = mrna.blocks.getIntrons // List([1:2000-3000:+], [1:4000-5000:+], [1:6000-7000:+])

// Note: all Region arithmetic operations are also available on Features