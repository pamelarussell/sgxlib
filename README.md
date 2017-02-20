# sgxlib
Genomics library for the Scala programming language

[![Coverage Status](https://coveralls.io/repos/github/pamelarussell/sgxlib/badge.svg?branch=master)](https://coveralls.io/github/pamelarussell/sgxlib?branch=master)
[![Build Status](https://travis-ci.org/pamelarussell/sgxlib.svg?branch=master)](https://travis-ci.org/pamelarussell/sgxlib)
[![Language: Scala](https://img.shields.io/badge/language-scala-blue.svg)](https://www.scala-lang.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-red.svg)](https://opensource.org/licenses/MIT)

## Overview

[sgxlib](https://github.com/pamelarussell/sgxlib) provides classes and methods for genomics applications. See [Overview of packages](#overview-of-packages) and [API documentation](http://pamelarussell.github.io/sgxlib/docs/api/) for details.

## Download

Go to [latest release](https://github.com/pamelarussell/sgxlib/releases/latest) for precompiled .jar file and source code downloads.

## Building from source

To build a fat .jar file, [sbt](http://www.scala-sbt.org/) is required.

```
sbt assembly
```

generates `target/scala-[version]/sgxlib-[release].jar`. The jar file can be imported as a library into Java or Scala projects.

## API documentation (Scaladoc)

View the [API docs](http://pamelarussell.github.io/sgxlib/docs/api/)

## Overview of packages

### Package [feature](http://pamelarussell.github.io/sgxlib/docs/api/#feature.package)

The `feature` package provides the concept of a genomic region with blocks and an orientation, as well as specific types of features such as transcripts and messenger RNAs. The package provides powerful feature arithmetic, with operations such as intersection, union, contains, minus, overlaps, and extraction of sub-features such as exons, introns, and sections of messenger RNAs.

Examples:

```scala
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
```

### Package [collection](http://pamelarussell.github.io/sgxlib/docs/api/#collection.package)

The `collection` package provides the concept of a collection of features, as well as methods to get the subcollection of features overlapping a given feature. An implementation is provided to create a feature collection from a [GTF2.2](http://mblab.wustl.edu/GTF22.html) annotation file.

Examples:

```scala
import java.io.File
import collection.{FeatureSet, GTF22FeatureSet}
import feature._

// Load a set of features from a GTF file
val featureSet: FeatureSet[Feature] =
  new GTF22FeatureSet(new File(getClass.getResource("/Homo_sapiens.GRCh38.86.chr_20_21_22.gtf").getPath))

// Get overlappers of an interval
val intervalOverlap: Iterator[Feature] = featureSet.overlappers("chr20", 37476591, 37645612, Plus) // ENST00000062104, ENST00000346199, ENST00000450588, ENST00000621390

// Get overlappers of a feature
val ENST00000613961: Feature = new Transcript(
  Block("20", 37499892, 37503975, Minus),
  Some("ENST00000613961"), Some("ENSG00000166619"))
val featureOverlap: Iterator[Feature] = featureSet.overlappers(ENST00000613961) // ENST00000613961, ENST00000467603
```

### Package [sequencing](http://pamelarussell.github.io/sgxlib/docs/api/#sequencing.package)

The `sequencing` package translates data in BAM files into the language of sgxlib, using the [samtools htsjdk](https://github.com/samtools/htsjdk) API under the hood. The `SamMapping` class expresses BAM records as sgxlib features. The `SamReader` class provides methods to query a BAM file by feature overlap. Feature overlap is defined strictly, requiring splice junctions and strand (where applicable) to match. This class also allows querying for full mapped fragments (matched mate pairs) where applicable.

Examples:

```scala
import java.io.File
import feature._
import htsjdk.samtools.SAMRecord
import sequencing.SamReader

// Construct a SamReader
val samReader: SamReader = new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath))

// Get iterator over all records
val nRec: Int = samReader.iterator.size // 45787

// Get fragments compatible with a feature
val frag: Iterator[(SAMRecord, SAMRecord)] =
  samReader.compatibleFragments(
    new GenericFeature(Block("20", 37500065, 37500486, Minus), None), Plus)
    /*
      (DRR023752.11381138 ,DRR023752.11381138)
      (DRR023752.16260396 ,DRR023752.16260396)
      (DRR023752.25576103 ,DRR023752.25576103)
      (DRR023752.34173869 ,DRR023752.34173869)
    */

// Get count of fragments compatible with a feature
val count: Int = samReader.countCompatibleFragments(
  new GenericFeature(Block("20", 37500065, 37500486, Unstranded), None), Unstranded) // 5

/* Note: the above methods for fragments are also available for single records,
regardless of whether the library is paired end. */
```

## Java interoperability

sgxlib can be used from within Java code. For example:

```java
package examples;

import feature.*;
import java.util.Arrays;
import scala.collection.JavaConverters;

/**
 * Example of using sgxlib classes from Java code
 */
public class HelloSgx {

    public static void main(String[] args) {

        // Create two Blocks on chr1
        Block blk1 = new Block("1", 1000, 2000, Plus$.MODULE$);
        Block blk2 = new Block("1", 3000, 4000, Plus$.MODULE$);
        System.out.println("The Blocks are " + blk1 + " and " + blk2);

        // Create a Region with the two blocks
        scala.collection.immutable.List<Block> bss =
                JavaConverters.asScalaBufferConverter(
                Arrays.asList(blk1, blk2))
                .asScala()
                .toList();
        BlockSet bs = new BlockSet(bss);
        System.out.println("The BlockSet is " + bs);

        // Create an mRNA from the Region
        scala.Option<String> noName = scala.Option.apply(null);
        scala.Option<String> geneName = scala.Option.apply("gene");
        MessengerRNA mrna = new MessengerRNA(bs, 1500, 3400, noName, geneName);
        System.out.println("The MessengerRNA is " + mrna);

        // Get the CDS
        Region cds = mrna.getCDS();
        System.out.println("The CDS is " + cds);

    }

}
```
