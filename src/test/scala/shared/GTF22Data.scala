package shared

import collection.{FeatureSet, GTF22FeatureSet}
import feature._

/**
  * Created by prussell on 12/28/16.
  */
object GTF22Data {

  val chr20_21_22: FeatureSet[Feature] =
    new GTF22FeatureSet(getClass.getResource("/Homo_sapiens.GRCh38.86.chr_20_21_22.gtf").getPath)

  // chr20:36996349-37095995
  val ENST00000373664: Feature = chr20_21_22.iterator.filter(_.name.contains("ENST00000373664")).toList.head

  // chr20:37003574-37095989
  val ENST00000344359: Feature = chr20_21_22.iterator.filter(_.name.contains("ENST00000344359")).toList.head

  // chr20:37056160-37068055
  val ENST00000525052: Feature = chr20_21_22.iterator.filter(_.name.contains("ENST00000525052")).toList.head

  // chr20:37289638-37317260
  val ENST00000373606: Feature = chr20_21_22.iterator.filter(_.name.contains("ENST00000373606")).toList.head

  // chr20:37289663-37317260
  val ENST00000397152: Feature = chr20_21_22.iterator.filter(_.name.contains("ENST00000397152")).toList.head

  val chr1_184922_185350_Minus = Block("1", 184922, 185350, Minus)	 // ENSG00000279457
  val chr1_184924_185350_Minus = Block("1", 184924, 185350, Minus)	 // ENSG00000279457
  val chr1_184926_184971_Minus = Block("1", 184926, 184971, Minus)	 // ENSG00000279457
  val chr1_184976_185049_Minus = Block("1", 184976, 185049, Minus)	 // ENSG00000279457
  val chr1_185490_185559_Minus = Block("1", 185490, 185559, Minus)	 // ENSG00000279457
  val chr1_185528_185559_Minus = Block("1", 185528, 185559, Minus)	 // ENSG00000279457
  val chr1_186316_186469_Minus = Block("1", 186316, 186469, Minus)	 // ENSG00000279457
  val chr1_187128_187267_Minus = Block("1", 187128, 187267, Minus)	 // ENSG00000279457
  val chr1_187128_187287_Minus = Block("1", 187128, 187287, Minus)	 // ENSG00000279457
  val chr1_187269_187287_Minus = Block("1", 187269, 187287, Minus)	 // ENSG00000279457
  val chr1_187375_187577_Minus = Block("1", 187375, 187577, Minus)	 // ENSG00000279457
  val chr1_187379_187577_Minus = Block("1", 187379, 187577, Minus)	 // ENSG00000279457
  val chr1_187754_187886_Minus = Block("1", 187754, 187886, Minus)	 // ENSG00000279457
  val chr1_188021_188028_Minus = Block("1", 188021, 188028, Minus)	 // ENSG00000279457
  val chr1_188101_188105_Minus = Block("1", 188101, 188105, Minus)	 // ENSG00000279457
  val chr1_188125_188266_Minus = Block("1", 188125, 188266, Minus)	 // ENSG00000279457
  val chr1_188129_188266_Minus = Block("1", 188129, 188266, Minus)	 // ENSG00000279457
  val chr1_188438_188486_Minus = Block("1", 188438, 188486, Minus)	 // ENSG00000279457
  val chr1_188488_188584_Minus = Block("1", 188488, 188584, Minus)	 // ENSG00000279457
  val chr1_188790_188889_Minus = Block("1", 188790, 188889, Minus)	 // ENSG00000279457
  val chr1_188790_188892_Minus = Block("1", 188790, 188892, Minus)	 // ENSG00000279457
  val chr1_188790_188902_Minus = Block("1", 188790, 188902, Minus)	 // ENSG00000279457
  val chr1_195258_195411_Minus = Block("1", 195258, 195411, Minus)	 // ENSG00000279457
  val chr1_195258_195416_Minus = Block("1", 195258, 195416, Minus)	 // ENSG00000279457
  val chr1_195262_195411_Minus = Block("1", 195262, 195411, Minus)	 // ENSG00000279457
  val chr1_200049_200322_Minus = Block("1", 200049, 200322, Minus)	 // ENSG00000279457
  val chr1_778769_779092_Plus = Block("1", 778769, 779092, Plus)	 // ENSG00000237491
  val chr1_778781_779092_Plus = Block("1", 778781, 779092, Plus)	 // ENSG00000237491
  val chr1_778929_779092_Plus = Block("1", 778929, 779092, Plus)	 // ENSG00000237491
  val chr1_778936_779092_Plus = Block("1", 778936, 779092, Plus)	 // ENSG00000237491
  val chr1_779055_779092_Plus = Block("1", 779055, 779092, Plus)	 // ENSG00000237491
  val chr1_781936_782043_Plus = Block("1", 781936, 782043, Plus)	 // ENSG00000237491
  val chr1_781936_782136_Plus = Block("1", 781936, 782136, Plus)	 // ENSG00000237491
  val chr1_781936_782191_Plus = Block("1", 781936, 782191, Plus)	 // ENSG00000237491
  val chr1_783110_783363_Plus = Block("1", 783110, 783363, Plus)	 // ENSG00000237491
  val chr1_784369_784429_Plus = Block("1", 784369, 784429, Plus)	 // ENSG00000237491
  val chr1_784369_784493_Plus = Block("1", 784369, 784493, Plus)	 // ENSG00000237491
  val chr1_784369_784655_Plus = Block("1", 784369, 784655, Plus)	 // ENSG00000237491
  val chr1_784369_784690_Plus = Block("1", 784369, 784690, Plus)	 // ENSG00000237491
  val chr1_784369_784759_Plus = Block("1", 784369, 784759, Plus)	 // ENSG00000237491
  val chr1_784369_784977_Plus = Block("1", 784369, 784977, Plus)	 // ENSG00000237491
  val chr1_784395_784493_Plus = Block("1", 784395, 784493, Plus)	 // ENSG00000237491
  val chr1_785799_787672_Plus = Block("1", 785799, 787672, Plus)	 // ENSG00000237491
  val chr1_786392_786866_Plus = Block("1", 786392, 786866, Plus)	 // ENSG00000237491
  val chr1_792881_793041_Plus = Block("1", 792881, 793041, Plus)	 // ENSG00000237491
  val chr1_795469_795513_Plus = Block("1", 795469, 795513, Plus)	 // ENSG00000237491
  val chr1_795469_795582_Plus = Block("1", 795469, 795582, Plus)	 // ENSG00000237491
  val chr1_801606_801876_Plus = Block("1", 801606, 801876, Plus)	 // ENSG00000237491
  val chr1_803566_803667_Plus = Block("1", 803566, 803667, Plus)	 // ENSG00000237491
  val chr1_803918_804222_Plus = Block("1", 803918, 804222, Plus)	 // ENSG00000237491
  val chr1_803921_804222_Plus = Block("1", 803921, 804222, Plus)	 // ENSG00000237491
  val chr1_803950_804222_Plus = Block("1", 803950, 804222, Plus)	 // ENSG00000237491
  val chr1_804775_804832_Plus = Block("1", 804775, 804832, Plus)	 // ENSG00000237491
  val chr1_804775_804875_Plus = Block("1", 804775, 804875, Plus)	 // ENSG00000237491
  val chr1_804775_804966_Plus = Block("1", 804775, 804966, Plus)	 // ENSG00000237491
  val chr1_804775_805127_Plus = Block("1", 804775, 805127, Plus)	 // ENSG00000237491
  val chr1_804775_805270_Plus = Block("1", 804775, 805270, Plus)	 // ENSG00000237491
  val chr1_804798_804966_Plus = Block("1", 804798, 804966, Plus)	 // ENSG00000237491
  val chr1_804931_804966_Plus = Block("1", 804931, 804966, Plus)	 // ENSG00000237491
  val chr1_806385_806459_Plus = Block("1", 806385, 806459, Plus)	 // ENSG00000237491
  val chr1_807216_807314_Plus = Block("1", 807216, 807314, Plus)	 // ENSG00000237491
  val chr1_807216_807321_Plus = Block("1", 807216, 807321, Plus)	 // ENSG00000237491
  val chr1_807216_807323_Plus = Block("1", 807216, 807323, Plus)	 // ENSG00000237491
  val chr1_807216_807465_Plus = Block("1", 807216, 807465, Plus)	 // ENSG00000237491
  val chr1_809621_810060_Plus = Block("1", 809621, 810060, Plus)	 // ENSG00000237491
  val chr1_809657_809729_Plus = Block("1", 809657, 809729, Plus)	 // ENSG00000237491
  val chr1_826205_827522_Minus = Block("1", 826205, 827522, Minus)	 // ENSG00000225880
  val chr1_868070_868675_Minus = Block("1", 868070, 868675, Minus)	 // ENSG00000230368
  val chr1_868239_868530_Minus = Block("1", 868239, 868530, Minus)	 // ENSG00000230368
  val chr1_868402_868675_Minus = Block("1", 868402, 868675, Minus)	 // ENSG00000230368
  val chr1_868626_868675_Minus = Block("1", 868626, 868675, Minus)	 // ENSG00000230368
  val chr1_869527_869575_Minus = Block("1", 869527, 869575, Minus)	 // ENSG00000230368
  val chr1_870085_870201_Minus = Block("1", 870085, 870201, Minus)	 // ENSG00000230368
  val chr1_874111_875155_Minus = Block("1", 874111, 875155, Minus)	 // ENSG00000230368
  val chr1_875039_875155_Minus = Block("1", 875039, 875155, Minus)	 // ENSG00000230368
  val chr1_876745_876802_Minus = Block("1", 876745, 876802, Minus)	 // ENSG00000230368
  val chr1_876745_876903_Minus = Block("1", 876745, 876903, Minus)	 // ENSG00000230368
  val chr1_960586_960800_Plus = Block("1", 960586, 960800, Plus)	 // ENSG00000187961
  val chr1_960638_960699_Plus = Block("1", 960638, 960699, Plus)	 // ENSG00000187961
  val chr1_961292_961552_Plus = Block("1", 961292, 961552, Plus)	 // ENSG00000187961
  val chr1_961448_961750_Plus = Block("1", 961448, 961750, Plus)	 // ENSG00000187961
  val chr1_961628_961750_Plus = Block("1", 961628, 961750, Plus)	 // ENSG00000187961
  val chr1_961825_962047_Plus = Block("1", 961825, 962047, Plus)	 // ENSG00000187961
  val chr1_961825_962478_Plus = Block("1", 961825, 962478, Plus)	 // ENSG00000187961
  val chr1_962081_962471_Plus = Block("1", 962081, 962471, Plus)	 // ENSG00000187961
  val chr1_962354_962471_Plus = Block("1", 962354, 962471, Plus)	 // ENSG00000187961
  val chr1_962703_962917_Plus = Block("1", 962703, 962917, Plus)	 // ENSG00000187961
  val chr1_962726_962917_Plus = Block("1", 962726, 962917, Plus)	 // ENSG00000187961
  val chr1_963031_963253_Plus = Block("1", 963031, 963253, Plus)	 // ENSG00000187961
  val chr1_963108_963253_Plus = Block("1", 963108, 963253, Plus)	 // ENSG00000187961
  val chr1_963336_963504_Plus = Block("1", 963336, 963504, Plus)	 // ENSG00000187961
  val chr1_963551_964008_Plus = Block("1", 963551, 964008, Plus)	 // ENSG00000187961
  val chr1_963919_964008_Plus = Block("1", 963919, 964008, Plus)	 // ENSG00000187961
  val chr1_964106_964164_Plus = Block("1", 964106, 964164, Plus)	 // ENSG00000187961
  val chr1_964106_964167_Plus = Block("1", 964106, 964167, Plus)	 // ENSG00000187961
  val chr1_964106_964180_Plus = Block("1", 964106, 964180, Plus)	 // ENSG00000187961
  val chr1_964348_964530_Plus = Block("1", 964348, 964530, Plus)	 // ENSG00000187961
  val chr1_964962_965602_Plus = Block("1", 964962, 965602, Plus)	 // ENSG00000187961
  val chr1_964962_965715_Plus = Block("1", 964962, 965715, Plus)	 // ENSG00000187961

  val ENST00000338591 = MessengerRNA(BlockSet(List(
    chr1_960586_960800_Plus,
    chr1_961292_961552_Plus,
    chr1_961628_961750_Plus,
    chr1_961825_962047_Plus,
    chr1_962354_962471_Plus,
    chr1_962703_962917_Plus,
    chr1_963108_963253_Plus,
    chr1_963336_963504_Plus,
    chr1_963919_964008_Plus,
    chr1_964106_964180_Plus,
    chr1_964348_964530_Plus,
    chr1_964962_965715_Plus
  )), 960693, 965191, Some("ENST00000338591"), Some("ENSG00000187961"))

  val ENST00000412115 = new Transcript(BlockSet(List(
    chr1_804931_804966_Plus,
    chr1_807216_807323_Plus,
    chr1_809621_810060_Plus
  )), Some("ENST00000412115"), Some("ENSG00000237491"))

  val ENST00000427857 = new Transcript(BlockSet(List(
    chr1_868402_868675_Minus,
    chr1_875039_875155_Minus,
    chr1_876745_876802_Minus
  )), Some("ENST00000427857"), Some("ENSG00000230368"))

  val ENST00000429505 = new Transcript(BlockSet(List(
    chr1_779055_779092_Plus,
    chr1_803918_804222_Plus,
    chr1_804775_804875_Plus
  )), Some("ENST00000429505"), Some("ENSG00000237491"))

  val ENST00000432963 = new Transcript(BlockSet(List(
    chr1_868239_868530_Minus,
    chr1_868626_868675_Minus,
    chr1_869527_869575_Minus,
    chr1_870085_870201_Minus
  )), Some("ENST00000432963"), Some("ENSG00000230368"))

  val ENST00000434264 = new Transcript(BlockSet(List(
    chr1_778769_779092_Plus,
    chr1_781936_782136_Plus,
    chr1_784369_784690_Plus
  )), Some("ENST00000434264"), Some("ENSG00000237491"))

  val ENST00000443772 = new Transcript(BlockSet(List(
    chr1_804798_804966_Plus,
    chr1_807216_807465_Plus
  )), Some("ENST00000443772"), Some("ENSG00000237491"))

  val ENST00000446136 = new Transcript(BlockSet(List(
    chr1_868070_868675_Minus,
    chr1_874111_875155_Minus,
    chr1_876745_876903_Minus
  )), Some("ENST00000446136"), Some("ENSG00000230368"))

  val ENST00000457084 = new Transcript(BlockSet(List(
    chr1_778781_779092_Plus,
    chr1_781936_782191_Plus
  )), Some("ENST00000457084"), Some("ENSG00000237491"))

  val ENST00000463212 = new Transcript(BlockSet(List(
    chr1_961448_961750_Plus,
    chr1_961825_962478_Plus
  )), Some("ENST00000463212"), Some("ENSG00000187961"))

  val ENST00000473798 = new Transcript(chr1_826205_827522_Minus, Some("ENST00000473798"), Some("ENSG00000225880"))

  val ENST00000481067 = new Transcript(BlockSet(List(
    chr1_963551_964008_Plus,
    chr1_964106_964164_Plus
  )), Some("ENST00000481067"), Some("ENSG00000187961"))

  val ENST00000585745 = new Transcript(BlockSet(List(
    chr1_784369_784493_Plus,
    chr1_795469_795513_Plus
  )), Some("ENST00000585745"), Some("ENSG00000237491"))

  val ENST00000585768 = new Transcript(BlockSet(List(
    chr1_784369_784493_Plus,
    chr1_801606_801876_Plus
  )), Some("ENST00000585768"), Some("ENSG00000237491"))

  val ENST00000585826 = new Transcript(chr1_784369_784759_Plus, Some("ENST00000585826"), Some("ENSG00000237491"))

  val ENST00000586288 = new Transcript(BlockSet(List(
    chr1_784395_784493_Plus,
    chr1_792881_793041_Plus,
    chr1_795469_795582_Plus,
    chr1_803566_803667_Plus,
    chr1_803950_804222_Plus,
    chr1_804775_804966_Plus,
    chr1_807216_807321_Plus
  )), Some("ENST00000586288"), Some("ENSG00000237491"))

  val ENST00000586928 = new Transcript(BlockSet(List(
    chr1_784369_784493_Plus,
    chr1_786392_786866_Plus
  )), Some("ENST00000586928"), Some("ENSG00000237491"))

  val ENST00000587530 = new Transcript(BlockSet(List(
    chr1_784369_784493_Plus,
    chr1_803566_803667_Plus,
    chr1_803918_804222_Plus,
    chr1_804775_805270_Plus
  )), Some("ENST00000587530"), Some("ENSG00000237491"))

  val ENST00000588951 = new Transcript(BlockSet(List(
    chr1_784369_784493_Plus,
    chr1_803918_804222_Plus,
    chr1_807216_807323_Plus,
    chr1_809657_809729_Plus
  )), Some("ENST00000588951"), Some("ENSG00000237491"))

  val ENST00000589531 = new Transcript(BlockSet(List(
    chr1_784369_784493_Plus,
    chr1_803921_804222_Plus,
    chr1_804775_804966_Plus,
    chr1_807216_807314_Plus
  )), Some("ENST00000589531"), Some("ENSG00000237491"))

  val ENST00000589899 = new Transcript(BlockSet(List(
    chr1_778929_779092_Plus,
    chr1_781936_782043_Plus,
    chr1_783110_783363_Plus,
    chr1_784369_784655_Plus
  )), Some("ENST00000589899"), Some("ENSG00000237491"))

  val ENST00000590848 = new Transcript(BlockSet(List(
    chr1_784369_784493_Plus,
    chr1_792881_793041_Plus,
    chr1_795469_795582_Plus,
    chr1_803950_804222_Plus,
    chr1_804775_804832_Plus
  )), Some("ENST00000590848"), Some("ENSG00000237491"))

  val ENST00000591440 = new Transcript(BlockSet(List(
    chr1_784369_784493_Plus,
    chr1_803918_804222_Plus,
    chr1_804775_805127_Plus
  )), Some("ENST00000591440"), Some("ENSG00000237491"))

  val ENST00000591702 = new Transcript(chr1_785799_787672_Plus, Some("ENST00000591702"), Some("ENSG00000237491"))

  val ENST00000592547 = new Transcript(chr1_784369_784977_Plus, Some("ENST00000592547"), Some("ENSG00000237491"))

  val ENST00000593022 = new Transcript(BlockSet(List(
    chr1_784369_784493_Plus,
    chr1_803918_804222_Plus,
    chr1_804775_804966_Plus,
    chr1_806385_806459_Plus
  )), Some("ENST00000593022"), Some("ENSG00000237491"))

  val ENST00000609830 = new Transcript(BlockSet(List(
    chr1_778936_779092_Plus,
    chr1_781936_782136_Plus,
    chr1_783110_783363_Plus,
    chr1_784369_784429_Plus
  )), Some("ENST00000609830"), Some("ENSG00000237491"))

  val ENST00000622660 = MessengerRNA(BlockSet(List(
    chr1_960638_960699_Plus,
    chr1_962081_962471_Plus,
    chr1_962703_962917_Plus,
    chr1_963108_963253_Plus,
    chr1_963336_963504_Plus,
    chr1_963919_964008_Plus,
    chr1_964106_964167_Plus,
    chr1_964348_964530_Plus,
    chr1_964962_965602_Plus
  )), 962706, 964352, Some("ENST00000622660"), Some("ENSG00000187961"))

  val ENST00000623083 = MessengerRNA(BlockSet(List(
    chr1_184924_185350_Minus,
    chr1_185490_185559_Minus,
    chr1_186316_186469_Minus,
    chr1_187128_187287_Minus,
    chr1_187375_187577_Minus,
    chr1_187754_187886_Minus,
    chr1_188125_188266_Minus,
    chr1_188438_188486_Minus,
    chr1_188488_188584_Minus,
    chr1_188790_188902_Minus,
    chr1_195262_195411_Minus
  )), 185216, 195411, Some("ENST00000623083"), Some("ENSG00000279457"))

  val ENST00000623834 = MessengerRNA(BlockSet(List(
    chr1_184922_185350_Minus,
    chr1_185490_185559_Minus,
    chr1_187375_187577_Minus,
    chr1_187754_187886_Minus,
    chr1_188021_188028_Minus,
    chr1_188129_188266_Minus,
    chr1_188438_188486_Minus,
    chr1_188488_188584_Minus,
    chr1_188790_188892_Minus,
    chr1_195258_195411_Minus
  )), 185216, 195411, Some("ENST00000623834"), Some("ENSG00000279457"))

  val ENST00000624735 = MessengerRNA(BlockSet(List(
    chr1_184926_184971_Minus,
    chr1_184976_185049_Minus,
    chr1_185528_185559_Minus,
    chr1_186316_186469_Minus,
    chr1_187128_187267_Minus,
    chr1_187269_187287_Minus,
    chr1_187379_187577_Minus,
    chr1_187754_187886_Minus,
    chr1_188101_188105_Minus,
    chr1_188129_188266_Minus,
    chr1_188438_188486_Minus,
    chr1_188488_188584_Minus,
    chr1_188790_188889_Minus,
    chr1_195258_195416_Minus,
    chr1_200049_200322_Minus
  )), 184926, 200086, Some("ENST00000624735"), Some("ENSG00000279457"))

  val ENST00000466300 = new Transcript(BlockSet(List(
    chr1_962726_962917_Plus,
    chr1_963031_963253_Plus,
    chr1_963336_963504_Plus,
    chr1_963919_964008_Plus,
    chr1_964106_964167_Plus,
    chr1_964348_964530_Plus
  )),
    Some("ENST00000466300"),
    Some("ENSG00000187961"))

  val transcript1400001 = MessengerRNA(BlockSet(List(
    Block("140", 65148, 65487, Minus),
    Block("140", 66822, 66999, Minus),
    Block("140", 70206, 70294, Minus),
    Block("140", 71695, 71807, Minus),
    Block("140", 73222, 73504, Minus)
  )), 66992, 71807, Some("140.000.1"), Some("140.000"))

  val inter_140_5140_8522_Minus = new GenericFeature(Block("140", 5140, 8522, Minus),
    Some("inter_140_5140_8522"))
  val inter_CNS_140_8522_9711_Minus = new GenericFeature(Block("140", 8522, 9711, Minus),
    Some("inter_CNS_140_8522_9711"))
  val inter_140_9711_13182_Minus = new GenericFeature(Block("140", 9711, 13182, Minus),
    Some("inter_140_9711_13182"))
  val intron_CNS_140_70102_70151_Minus = new GenericFeature(Block("140", 70102, 70151, Minus)
    , Some("intron_CNS_140_70102_70151_140.000.1"))

}
