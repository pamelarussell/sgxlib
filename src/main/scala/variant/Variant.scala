package variant

import htsjdk.variant.variantcontext._
import scala.collection.JavaConverters._

/**
  * A mirror of a [[VariantContext]] that can be instantiated
  * @param source
  * @param id
  * @param contig
  * @param start
  * @param stop
  * @param alleles
  * @param genotypes
  * @param log10pError
  * @param filters
  * @param attributes
  * @param fullyDecoded
  */
private final class InstantiableVariantContext(source: String,
                                       id: String,
                                       contig: String,
                                       start: Long,
                                       stop: Long,
                                       alleles: java.util.Collection[Allele],
                                       genotypes: GenotypesContext,
                                       log10pError: Double,
                                       filters: java.util.Set[String],
                                       attributes: java.util.Map[String, Object],
                                       fullyDecoded: Boolean)
  extends VariantContext(source, id, contig, start, stop, alleles, genotypes, log10pError, filters,
    attributes, fullyDecoded, java.util.EnumSet.of(VariantContext.Validation.ALLELES, VariantContext.Validation.GENOTYPES)) {

  //super.validateAlternateAlleles()
  super.validateChromosomeCounts()

}

/**
  * Class representing the data contents of a [[VariantContext]].
  * Kind of like a builder but with limited mutation capabilities.
  * @param vc [[VariantContext]] to get data from initially
  */
private final class VariantContextData(val vc: VariantContext) {

  val source: String = vc.getSource
  val id: String = vc.getID
  val contig: String = vc.getContig
  val start: Long = vc.getStart
  val stop: Long = vc.getEnd
  val alleles: java.util.Collection[Allele] = vc.getAlleles
  val genotypes: GenotypesContext = vc.getGenotypes
  val log10pError: Double = vc.getLog10PError
  val filters: java.util.Set[String] = vc.getFilters
  val attributes: java.util.Map[String, Object] = vc.getAttributes
  val fullyDecoded: Boolean = vc.isFullyDecoded

  def this(source: String,
           id: String,
           contig: String,
           start: Long,
           stop: Long,
           alleles: java.util.Collection[Allele],
           genotypes: GenotypesContext,
           log10pError: Double,
           filters: java.util.Set[String],
           attributes: java.util.Map[String, Object],
           fullyDecoded: Boolean) = this(new InstantiableVariantContext(source, id, contig, start, stop, alleles,
    genotypes, log10pError, filters, attributes, fullyDecoded))

  /**
    * Returns a new [[VariantContextData]] object with the genotypes modified
    * @param newGenotypes New genotypes
    * @return New [[VariantContextData]] created from this one with modified genotypes
    */
  def setGenotypes(newGenotypes: GenotypesContext): VariantContextData = {
    val newAlleles: java.util.Collection[Allele] = asJavaCollection(
      asScalaIterator(newGenotypes.iterator()).
        flatMap[Allele](genotype => asScalaIterator(genotype.getAlleles.iterator())
        .filter(allele => allele.isCalled))
        .toSet)
    new VariantContextData(source, id, contig, start, stop, newAlleles, newGenotypes, log10pError, filters, attributes, fullyDecoded)
  }

  /**
    * Returns a new [[VariantContextData]] object with the allele list modified
    * @param alleleList New set of alleles
    * @return [[VariantContextData]] created from this one with modified alleles
    */
  def setAlleleList(alleleList: java.util.Collection[Allele]) = new VariantContextData(source, id, contig, start, stop, alleleList,
    genotypes, log10pError, filters, attributes, fullyDecoded)

  /**
    * Get a new [[VariantContext]] with the data contained herein
    * @return New [[VariantContext]] built from the data in this object
    */
  def getVariantContext: VariantContext = new InstantiableVariantContext(source, id, contig, start, stop, alleles,
    genotypes, log10pError, filters, attributes, fullyDecoded)

}

/**
  * Utilities for [[VariantContext]]s
  */
object VariantContextUtil {

  /**
    * Returns the names of samples whose genotypes include a given allele
    * @param vc [[VariantContext]]
    * @param allele Allele
    * @return Sample names that have at least one copy of the allele in the given [[VariantContext]]
    */
  def samplesWithAllele(vc: VariantContext, allele: Allele): Iterable[String] = {

    def hasAllele(genotype: Genotype) = {
      asScalaIterator(genotype.getAlleles.iterator()).
        exists(a => a.equals(allele, true))
    }

    asScalaIterator(vc.getGenotypes.iterator()).
      filter(hasAllele).
      map(genotype => genotype.getSampleName).
      toSet
  }

}

/**
  * Functions to manipulate [[VariantContext]]s
  */
object VariantContextMutations {

  /**
    * Returns a new [[VariantContext]] with one individual genotype changed
    * @param vc Original [[VariantContext]]
    * @param sampleId Sample ID for genotype to change
    * @param newAlleles New alleles for the sample
    * @return New [[VariantContext]] with the individual genotype changed
    */
  private def setGenotype(vc: VariantContext, sampleId: String, newAlleles: java.util.List[Allele]): VariantContext = {
    if(!vc.hasGenotype(sampleId)) throw new IllegalArgumentException(s"VCF record does not have sample: $sampleId")
    val origGenotype: Genotype = vc.getGenotype(sampleId)
    val vcData = new VariantContextData(vc)
    val newGenotype: Genotype = new GenotypeBuilder(origGenotype).alleles(newAlleles).make()
    val newGenotypesContext: GenotypesContext = GenotypesContext.copy(vc.getGenotypes)
    newGenotypesContext.replace(newGenotype)
    val newVcData: VariantContextData = vcData.setGenotypes(newGenotypesContext)
    newVcData.getVariantContext
  }

  /**
    * Returns a new [[VariantContext]] with one individual genotype set to missing
    * @param vc Original [[VariantContext]]
    * @param sampleId Sample ID for genotype to set to missing
    * @return New [[VariantContext]] with the individual genotype set to missing
    */
  def setGenotypeToMissing(vc: VariantContext, sampleId: String): VariantContext = {
    val allelesMissing: java.util.List[Allele] = java.util.Arrays.asList(Allele.NO_CALL, Allele.NO_CALL)
    setGenotype(vc, sampleId, allelesMissing)
  }

  /**
    * Returns a new [[VariantContext]] with genotypes set to missing for any individual
    * who has at least one copy of a given allele. The given allele is also removed from
    * the list of alleles. If no individuals have the allele, no individual genotypes
    * are changed but the allele is still removed from the list of alleles.
    * @param vc Original [[VariantContext]]
    * @param allele Allele
    * @return New [[VariantContext]] with individual genotypes set to missing for any
    *         individual with at least one copy of the allele, and the allele removed from
    *         the list of alleles
    */
  def removeAllele(vc: VariantContext, allele: Allele): VariantContext = {
    val samplesWithAllele = VariantContextUtil.samplesWithAllele(vc, allele)
    val vcGenotypesModified: VariantContext = samplesWithAllele.foldLeft[VariantContext](vc)((v, s) => setGenotypeToMissing(v, s))
    val newAlleleList = vc.getAlleles
    newAlleleList.remove(allele)
    new VariantContextData(vcGenotypesModified)
        .setAlleleList(newAlleleList)
        .getVariantContext
  }

}

