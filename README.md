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

`sbt assembly`

generates `target/scala-[version]/sgxlib-[release].jar`. The jar file can be imported as a library into Java or Scala projects.

## API documentation (Scaladoc)

View the [API docs](http://pamelarussell.github.io/sgxlib/docs/api/)

## Overview of packages

TBA

## Java interoperability

As with all Scala libraries, sgxlib can be used from within Java code. For example:

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
