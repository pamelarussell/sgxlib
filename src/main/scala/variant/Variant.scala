package variant

import htsjdk.variant.variantcontext._

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

  super.validateAlternateAlleles()
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
  def setGenotypes(newGenotypes: GenotypesContext): VariantContextData = new VariantContextData(source, id, contig, start, stop, alleles,
    newGenotypes, log10pError, filters, attributes, fullyDecoded)

  /**
    * Get a new [[VariantContext]] with the data contained herein
    * @return New [[VariantContext]] built from the data in this object
    */
  def getVariantContext: VariantContext = new InstantiableVariantContext(source, id, contig, start, stop, alleles,
    genotypes, log10pError, filters, attributes, fullyDecoded)

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
    newGenotypesContext.add(newGenotype)
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

}

