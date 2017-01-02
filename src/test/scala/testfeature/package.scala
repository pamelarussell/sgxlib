import feature._

/**
  * Created by prussell on 10/1/16.
  */
package object testfeature {

  // Single blocks
    val chr1_500_600_plus = Block("1", 500, 600, Plus)
    val chr1_1000_2000_minus = Block("1", 1000, 2000, Minus)
    val chr1_1000_2000_both = Block("1", 1000, 2000, Both)
    val chr1_1000_2000_unstranded = Block("1", 1000, 2000, Unstranded)
    val chr1_1000_2000_plus_1 = Block("1", 1000, 2000, Plus)
    val chr1_1000_2000_plus_2 = Block("1", 1000, 2000, Plus)
    val chr1_1000_2000_plus_3 = Block("1", 1000, 2000, Plus)
    val chr1_1200_1300_plus = Block("1", 1200, 1300, Plus)
    val chr1_1500_2500_plus = Block("1", 1500, 2500, Plus)
    val chr1_1500_2500_minus = Block("1", 1500, 2500, Minus)
    val chr1_1500_2500_both = Block("1", 1500, 2500, Both)
    val chr1_1500_2500_unstranded = Block("1", 1500, 2500, Unstranded)
    val chr1_2000_3000_plus = Block("1", 2000, 3000, Plus)
    val chr2_1000_2000_plus = Block("2", 1000, 2000, Plus)
    val chr2_3000_4000_plus = Block("2", 3000, 4000, Plus)
    val chr1_3000_4000_plus = Block("1", 3000, 4000, Plus)
    val chr2_3000_4000_both = Block("2", 3000, 4000, Both)
    val chr1_3000_4000_minus = Block("1", 3000, 4000, Minus)
    val chr1_5_6_minus = Block("1", 5, 6, Minus)
    val chr1_4_6_minus = Block("1", 4, 6, Minus)
    val chr1_6_7_minus = Block("1", 6, 7, Minus)
    val chr1_1000_2000_plus = Block("1", 1000, 2000, Plus)

    // Block sets
    val chr1_100_200_300_400_plus = BlockSet(List(
      Block("1", 100, 200, Plus),
      Block("1", 300, 400, Plus)
    ))
   val chr1_100_200_300_400_minus = BlockSet(List(
      Block("1", 100, 200, Minus),
      Block("1", 300, 400, Minus)
    ))
    val chr1_900_1100_1200_1300_plus = BlockSet(List(
      Block("1", 900, 1100, Plus),
      Block("1", 1200, 1300, Plus)
    ))
    val chr1_900_2100_3000_4000_plus = BlockSet(List(
      Block("1", 900, 2100, Plus),
      Block("1", 3000, 4000, Plus)
    ))
    val chr1_1100_1200_1300_1400_plus = BlockSet(List(
      Block("1", 1100, 1200, Plus),
      Block("1", 1300, 1400, Plus)
    ))
    val chr1_1100_1200_1300_1400_1500_1600_plus = BlockSet(List(
      Block("1", 1100, 1200, Plus),
      Block("1", 1300, 1400, Plus),
      Block("1", 1500, 1600, Plus)
    ))
    val chr1_1000_1100_1200_1300_plus = BlockSet(List(
      Block("1", 1000, 1100, Plus),
      Block("1", 1200, 1300, Plus)
    ))
    val chr1_1000_1100_1900_2000_plus = BlockSet(List(
      Block("1", 1000, 1100, Plus),
      Block("1", 1900, 2000, Plus)
    ))
    val chr1_1000_1100_1200_1300_1900_2000_plus = BlockSet(List(
      Block("1", 1000, 1100, Plus),
      Block("1", 1200, 1300, Plus),
      Block("1", 1900, 2000, Plus)
    ))
    val chr1_1900_2100_3000_4000_plus = BlockSet(List(
      Block("1", 1900, 2100, Plus),
      Block("1", 3000, 4000, Plus)
    ))
    val chr1_1700_1800_1900_2100_plus = BlockSet(List(
      Block("1", 1700, 1800, Plus),
      Block("1", 1900, 2100, Plus)
    ))
    val chr1_1500_1600_2100_2300_plus = BlockSet(List(
      Block("1", 1500, 1600, Plus),
      Block("1", 2100, 2300, Plus)
    ))
    val chr1_1500_1600_1700_1800_2100_2300_plus = BlockSet(List(
      Block("1", 1500, 1600, Plus),
      Block("1", 1700, 1800, Plus),
      Block("1", 2100, 2300, Plus)
    ))
    val chr1_900_1100_1500_1600_1900_2100_plus = BlockSet(List(
      Block("1", 900, 1100, Plus),
      Block("1", 1500, 1600, Plus),
      Block("1", 1900, 2100, Plus)
    ))
    val chr1_1900_2000_3000_4000_plus = BlockSet(List(
      Block("1", 1900, 2000, Plus),
      Block("1", 3000, 4000, Plus)
    ))
    val chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus = BlockSet(List(
      Block("1", 1000, 2000, Minus),
      Block("1", 3000, 4000, Minus),
      Block("1", 5000, 6000, Minus),
      Block("1", 7000, 8000, Minus)
    ))
    val chr1_1000_2000_2500_3000_4000_5000_6000_6500_7000_8000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 2500, 3000, Plus),
      Block("1", 4000, 5000, Plus),
      Block("1", 6000, 6500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_1000_2100_2500_3000_4000_5000_6000_6500_7000_8000_plus = BlockSet(List(
      Block("1", 1000, 2100, Plus),
      Block("1", 2500, 3000, Plus),
      Block("1", 4000, 5000, Plus),
      Block("1", 6000, 6500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_1000_2000_2400_3000_4000_5000_6000_6500_7000_8000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 2400, 3000, Plus),
      Block("1", 4000, 5000, Plus),
      Block("1", 6000, 6500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_1000_2000_2600_3000_4000_5000_6000_6500_7000_8000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 2600, 3000, Plus),
      Block("1", 4000, 5000, Plus),
      Block("1", 6000, 6500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_1000_2000_4000_5000_6000_6500_6600_6700_7000_8000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 4000, 5000, Plus),
      Block("1", 6000, 6500, Plus),
      Block("1", 6600, 6700, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_1000_2000_2500_3000_4000_5000_6000_6500_7100_8000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 2500, 3000, Plus),
      Block("1", 4000, 5000, Plus),
      Block("1", 6000, 6500, Plus),
      Block("1", 7100, 8000, Plus)
    ))
    val chr1_1000_2000_2500_3000_4000_5000_6000_6600_7000_8000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 2500, 3000, Plus),
      Block("1", 4000, 5000, Plus),
      Block("1", 6000, 6600, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_1000_2000_2500_6500_7000_8000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 2500, 6500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_1000_2000_3000_4000_5000_6000_7000_8000_both = BlockSet(List(
      Block("1", 1000, 2000, Both),
      Block("1", 3000, 4000, Both),
      Block("1", 5000, 6000, Both),
      Block("1", 7000, 8000, Both)
    ))
  val chr1_1000_2000_3000_4000_5000_6000_7000_8000_unstranded = BlockSet(List(
      Block("1", 1000, 2000, Unstranded),
      Block("1", 3000, 4000, Unstranded),
      Block("1", 5000, 6000, Unstranded),
      Block("1", 7000, 8000, Unstranded)
    ))
    val chr1_1000_2000_3000_4000_5000_6000_both = BlockSet(List(
      Block("1", 1000, 2000, Both),
      Block("1", 3000, 4000, Both),
      Block("1", 5000, 6000, Both)
    ))
    val chr1_1000_2000_3000_4000_5000_6000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus)
    ))
    val chr1_1000_2000_3000_4000_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus)
    ))
    val chr1_1000_2000_3500_3600_plus = BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3500, 3600, Plus)
    ))
    val chr1_1000_2500_6000_7100_7500_8000_9000_10000_plus = BlockSet(List(
      Block("1", 1000, 2500, Plus),
      Block("1", 6000, 7100, Plus),
      Block("1", 7500, 8000, Plus),
      Block("1", 9000, 10000, Plus)
    ))
    val chr1_1000_2500_7000_7100_7500_8000_9000_10000_plus = BlockSet(List(
      Block("1", 1000, 2500, Plus),
      Block("1", 7000, 7100, Plus),
      Block("1", 7500, 8000, Plus),
      Block("1", 9000, 10000, Plus)
    ))
    val chr1_1000_3000_3500_3600_plus = BlockSet(List(
      Block("1", 1000, 3000, Plus),
      Block("1", 3500, 3600, Plus)
    ))
    val chr1_1000_4000_7000_8000_minus = BlockSet(List(
      Block("1", 1000, 4000, Minus),
      Block("1", 7000, 8000, Minus)
    ))
    val chr1_1100_1200_1300_1400_3500_3600_6000_7000_plus = BlockSet(List(
      Block("1", 1100, 1200, Plus),
      Block("1", 1300, 1400, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 6000, 7000, Plus)
    ))
    val chr1_1100_1200_1300_1400_3500_3600_plus = BlockSet(List(
      Block("1", 1100, 1200, Plus),
      Block("1", 1300, 1400, Plus),
      Block("1", 3500, 3600, Plus)
    ))
    val chr1_1100_1200_2500_3000_both = BlockSet(List(
      Block("1", 1100, 1200, Both),
      Block("1", 2500, 3000, Both)
    ))
    val chr1_1100_1200_4000_4600_both = BlockSet(List(
      Block("1", 1100, 1200, Both),
      Block("1", 4000, 4600, Both)
    ))
    val chr1_1500_1600_2000_6500_7500_7600_7700_7800_plus = BlockSet(List(
      Block("1", 1500, 1600, Plus),
      Block("1", 2000, 6500, Plus),
      Block("1", 7500, 7600, Plus),
      Block("1", 7700, 7800, Plus)
    ))
    val chr1_1500_1600_2500_4500_7500_7600_8000_10000_plus = BlockSet(List(
      Block("1", 1500, 1600, Plus),
      Block("1", 2500, 4500, Plus),
      Block("1", 7500, 7600, Plus),
      Block("1", 8000, 10000, Plus)
    ))
    val chr1_1500_1600_2500_4500_7500_7600_9000_10000_plus = BlockSet(List(
      Block("1", 1500, 1600, Plus),
      Block("1", 2500, 4500, Plus),
      Block("1", 7500, 7600, Plus),
      Block("1", 9000, 10000, Plus)
    ))
    val chr1_1500_1600_2500_6500_7500_7600_7700_7800_plus = BlockSet(List(
      Block("1", 1500, 1600, Plus),
      Block("1", 2500, 6500, Plus),
      Block("1", 7500, 7600, Plus),
      Block("1", 7700, 7800, Plus)
    ))
    val chr1_1500_1600_3500_3600_5500_6500_plus = BlockSet(List(
      Block("1", 1500, 1600, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 5500, 6500, Plus)
    ))
    val chr1_1500_2400_2600_3500_plus = BlockSet(List(
      Block("1", 1500, 2400, Plus),
      Block("1", 2600, 3500, Plus)
    ))
    val chr1_1500_2500_3000_4500_5000_6000_plus = BlockSet(List(
      Block("1", 1500, 2500, Plus),
      Block("1", 3000, 4500, Plus),
      Block("1", 5000, 6000, Plus)
    ))
    val chr1_1500_2500_3500_4500_5000_6000_plus = BlockSet(List(
      Block("1", 1500, 2500, Plus),
      Block("1", 3500, 4500, Plus),
      Block("1", 5000, 6000, Plus)
    ))
    val chr1_1500_2500_3500_4500_plus = BlockSet(List(
      Block("1", 1500, 2500, Plus),
      Block("1", 3500, 4500, Plus)
    ))
    val chr1_1500_3500_3600_3700_3800_3900_plus = BlockSet(List(
      Block("1", 1500, 3500, Plus),
      Block("1", 3600, 3700, Plus),
      Block("1", 3800, 3900, Plus)
    ))
    val chr1_1500_3500_3600_3700_3800_5000_plus = BlockSet(List(
      Block("1", 1500, 3500, Plus),
      Block("1", 3600, 3700, Plus),
      Block("1", 3800, 5000, Plus)
    ))
    val chr1_1500_5500_6000_8000_plus = BlockSet(List(
      Block("1", 1500, 5500, Plus),
      Block("1", 6000, 8000, Plus)
    ))
    val chr1_1500_5500_7000_8000_plus = BlockSet(List(
      Block("1", 1500, 5500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_2000_2600_2800_9000_plus = BlockSet(List(
      Block("1", 2000, 2600, Plus),
      Block("1", 2800, 9000, Plus)
    ))
    val chr1_2000_2600_3500_3600_3800_4500_plus = BlockSet(List(
      Block("1", 2000, 2600, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 3800, 4500, Plus)
    ))
    val chr1_2000_2600_3500_3600_6500_6600_plus = BlockSet(List(
      Block("1", 2000, 2600, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 6500, 6600, Plus)
    ))
    val chr1_2000_3000_3500_3600_4500_4600_plus = BlockSet(List(
      Block("1", 2000, 3000, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 4500, 4600, Plus)
    ))
    val chr1_2000_3000_3500_3600_5500_5600_7500_7600_8500_9500_plus = BlockSet(List(
      Block("1", 2000, 3000, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 5500, 5600, Plus),
      Block("1", 7500, 7600, Plus),
      Block("1", 8500, 9500, Plus)
    ))
    val chr1_2000_3000_3500_3600_both = BlockSet(List(
      Block("1", 2000, 3000, Both),
      Block("1", 3500, 3600, Both)
    ))
    val chr1_2000_3000_4000_5000_5500_5600_plus = BlockSet(List(
      Block("1", 2000, 3000, Plus),
      Block("1", 4000, 5000, Plus),
      Block("1", 5500, 5600, Plus)
    ))
    val chr1_2000_3000_4000_5000_6000_7000_both = BlockSet(List(
      Block("1", 2000, 3000, Both),
      Block("1", 4000, 5000, Both),
      Block("1", 6000, 7000, Both)
    ))
    val chr1_2000_3000_4000_5000_8000_9000_both = BlockSet(List(
      Block("1", 2000, 3000, Both),
      Block("1", 4000, 5000, Both),
      Block("1", 8000, 9000, Both)
    ))
    val chr1_2000_3000_4000_5000_plus = BlockSet(List(
      Block("1", 2000, 3000, Plus),
      Block("1", 4000, 5000, Plus)
    ))
    val chr1_2000_3000_8000_9000_both = BlockSet(List(
      Block("1", 2000, 3000, Both),
      Block("1", 8000, 9000, Both)
    ))
    val chr1_2000_4500_5000_5100_5900_6000_plus = BlockSet(List(
      Block("1", 2000, 4500, Plus),
      Block("1", 5000, 5100, Plus),
      Block("1", 5900, 6000, Plus)
    ))
    val chr1_2000_4500_6500_8500_plus = BlockSet(List(
      Block("1", 2000, 4500, Plus),
      Block("1", 6500, 8500, Plus)
    ))
    val chr1_2000_6000_7000_8000_plus = BlockSet(List(
      Block("1", 2000, 6000, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_2000_8500_9000_10000_plus = BlockSet(List(
      Block("1", 2000, 8500, Plus),
      Block("1", 9000, 10000, Plus)
    ))
    val chr1_2100_2200_2300_2400_2500_3100_plus = BlockSet(List(
      Block("1", 2100, 2200, Plus),
      Block("1", 2300, 2400, Plus),
      Block("1", 2500, 3100, Plus)
    ))
    val chr1_2100_2200_2300_3000_4000_5000_plus = BlockSet(List(
      Block("1", 2100, 2200, Plus),
      Block("1", 2300, 3000, Plus),
      Block("1", 4000, 5000, Plus)
    ))
    val chr1_1500_2500_4000_5000_plus = BlockSet(List(
      Block("1", 1500, 2500, Plus),
      Block("1", 4000, 5000, Plus)
    ))
    val chr1_2500_2600_2700_3500_6000_7000_plus = BlockSet(List(
      Block("1", 2500, 2600, Plus),
      Block("1", 2700, 3500, Plus),
      Block("1", 6000, 7000, Plus)
    ))
    val chr1_2500_2600_2800_9000_plus = BlockSet(List(
      Block("1", 2500, 2600, Plus),
      Block("1", 2800, 9000, Plus)
    ))
    val chr1_2500_2600_3500_3600_3800_4500_plus = BlockSet(List(
      Block("1", 2500, 2600, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 3800, 4500, Plus)
    ))
    val chr1_2500_2600_3500_3600_4500_4600_plus = BlockSet(List(
      Block("1", 2500, 2600, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 4500, 4600, Plus)
    ))
    val chr1_2500_2600_3500_3600_6500_6600_plus = BlockSet(List(
      Block("1", 2500, 2600, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 6500, 6600, Plus)
    ))
    val chr1_2500_2600_3500_5500_6500_6600_8500_8600_plus = BlockSet(List(
      Block("1", 2500, 2600, Plus),
      Block("1", 3500, 5500, Plus),
      Block("1", 6500, 6600, Plus),
      Block("1", 8500, 8600, Plus)
    ))
    val chr1_2500_2600_4500_4600_5000_6000_plus = BlockSet(List(
      Block("1", 2500, 2600, Plus),
      Block("1", 4500, 4600, Plus),
      Block("1", 5000, 6000, Plus)
    ))
    val chr1_2500_2600_4500_4600_6500_6600_both = BlockSet(List(
      Block("1", 2500, 2600, Both),
      Block("1", 4500, 4600, Both),
      Block("1", 6500, 6600, Both)
    ))
    val chr1_2500_2600_5500_5600_6500_6600_plus = BlockSet(List(
      Block("1", 2500, 2600, Plus),
      Block("1", 5500, 5600, Plus),
      Block("1", 6500, 6600, Plus)
    ))
    val chr1_2500_3000_3500_5500_6500_6600_8500_8600_plus = BlockSet(List(
      Block("1", 2500, 3000, Plus),
      Block("1", 3500, 5500, Plus),
      Block("1", 6500, 6600, Plus),
      Block("1", 8500, 8600, Plus)
    ))
    val chr1_2500_3000_4500_4600_5000_6000_plus = BlockSet(List(
      Block("1", 2500, 3000, Plus),
      Block("1", 4500, 4600, Plus),
      Block("1", 5000, 6000, Plus)
    ))
    val chr1_2500_3000_5500_5600_plus = BlockSet(List(
      Block("1", 2500, 3000, Plus),
      Block("1", 5500, 5600, Plus)
    ))
    val chr1_2500_3500_5500_5600_plus = BlockSet(List(
      Block("1", 2500, 3500, Plus),
      Block("1", 5500, 5600, Plus)
    ))
    val chr1_2500_3500_7000_8000_plus = BlockSet(List(
      Block("1", 2500, 3500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_2500_3000_8000_9000_both = BlockSet(List(
      Block("1", 2500, 3000, Both),
      Block("1", 8000, 9000, Both)
    ))
    val chr1_2500_4500_6500_8500_plus = BlockSet(List(
      Block("1", 2500, 4500, Plus),
      Block("1", 6500, 8500, Plus)
    ))
    val chr1_2500_5000_5500_5600_plus = BlockSet(List(
      Block("1", 2500, 5000, Plus),
      Block("1", 5500, 5600, Plus)
    ))
    val chr1_2500_8500_9000_10000_plus = BlockSet(List(
      Block("1", 2500, 8500, Plus),
      Block("1", 9000, 10000, Plus)
    ))
    val chr1_3000_3100_3200_3300_4500_4600_plus = BlockSet(List(
      Block("1", 3000, 3100, Plus),
      Block("1", 3200, 3300, Plus),
      Block("1", 4500, 4600, Plus)
    ))
    val chr1_3000_3100_3200_3300_4500_5000_plus = BlockSet(List(
      Block("1", 3000, 3100, Plus),
      Block("1", 3200, 3300, Plus),
      Block("1", 4500, 5000, Plus)
    ))
    val chr1_3000_4000_4500_5500_plus = BlockSet(List(
      Block("1", 3000, 4000, Plus),
      Block("1", 4500, 5500, Plus)
    ))
    val chr1_3000_4000_5500_6500_plus = BlockSet(List(
      Block("1", 3000, 4000, Plus),
      Block("1", 5500, 6500, Plus)
    ))
    val chr1_3000_4500_5500_5600_7000_8000_plus = BlockSet(List(
      Block("1", 3000, 4500, Plus),
      Block("1", 5500, 5600, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_3000_6000_7000_8000_plus = BlockSet(List(
      Block("1", 3000, 6000, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_3500_3600_4000_6000_plus = BlockSet(List(
      Block("1", 3500, 3600, Plus),
      Block("1", 4000, 6000, Plus)
    ))
    val chr1_3500_3600_4500_6000_plus = BlockSet(List(
      Block("1", 3500, 3600, Plus),
      Block("1", 4500, 6000, Plus)
    ))
    val chr1_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus = BlockSet(List(
      Block("1", 3500, 3600, Plus),
      Block("1", 5100, 5200, Plus),
      Block("1", 5300, 5400, Plus),
      Block("1", 6500, 6600, Plus),
      Block("1", 8500, 8600, Plus)
    ))
    val chr1_3500_3600_5500_5600_7500_7600_8500_9500_plus = BlockSet(List(
      Block("1", 3500, 3600, Plus),
      Block("1", 5500, 5600, Plus),
      Block("1", 7500, 7600, Plus),
      Block("1", 8500, 9500, Plus)
    ))
    val chr1_3500_4500_5000_5100_5900_6000_plus = BlockSet(List(
      Block("1", 3500, 4500, Plus),
      Block("1", 5000, 5100, Plus),
      Block("1", 5900, 6000, Plus)
    ))
    val chr1_3500_4500_5500_5600_7000_8000_plus = BlockSet(List(
      Block("1", 3500, 4500, Plus),
      Block("1", 5500, 5600, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_3500_4500_5500_6500_9000_10000_plus = BlockSet(List(
      Block("1", 3500, 4500, Plus),
      Block("1", 5500, 6500, Plus),
      Block("1", 9000, 10000, Plus)
    ))
    val chr1_3500_5000_5500_6500_9000_10000_plus = BlockSet(List(
      Block("1", 3500, 5000, Plus),
      Block("1", 5500, 6500, Plus),
      Block("1", 9000, 10000, Plus)
    ))
    val chr1_3500_5000_6000_7000_plus = BlockSet(List(
      Block("1", 3500, 5000, Plus),
      Block("1", 6000, 7000, Plus)
    ))
    val chr1_3500_5500_6000_8000_plus = BlockSet(List(
      Block("1", 3500, 5500, Plus),
      Block("1", 6000, 8000, Plus)
    ))
    val chr1_3500_5500_7000_8000_plus = BlockSet(List(
      Block("1", 3500, 5500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_4000_4600_5500_5600_plus = BlockSet(List(
      Block("1", 4000, 4600, Plus),
      Block("1", 5500, 5600, Plus)
    ))
    val chr1_4000_5000_5500_5600_6000_6600_plus = BlockSet(List(
      Block("1", 4000, 5000, Plus),
      Block("1", 5500, 5600, Plus),
      Block("1", 6000, 6600, Plus)
    ))
    val chr1_4500_4600_5500_6500_7000_8000_plus = BlockSet(List(
      Block("1", 4500, 4600, Plus),
      Block("1", 5500, 6500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_4500_4600_6500_6600_8500_8600_both = BlockSet(List(
      Block("1", 4500, 4600, Both),
      Block("1", 6500, 6600, Both),
      Block("1", 8500, 8600, Both)
    ))
    val chr1_4500_5000_6500_6600_both = BlockSet(List(
      Block("1", 4500, 5000, Both),
      Block("1", 6500, 6600, Both)
    ))
    val chr1_4500_5500_6500_7500_plus = BlockSet(List(
      Block("1", 4500, 5500, Plus),
      Block("1", 6500, 7500, Plus)
    ))
    val chr1_4500_6000_6500_7500_plus = BlockSet(List(
      Block("1", 4500, 6000, Plus),
      Block("1", 6500, 7500, Plus)
    ))
    val chr1_5000_6000_7000_8000_plus = BlockSet(List(
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_500_1000_1500_1600_2500_2600_4500_4600_5000_6000_plus = BlockSet(List(
      Block("1", 500, 1000, Plus),
      Block("1", 1500, 1600, Plus),
      Block("1", 2500, 2600, Plus),
      Block("1", 4500, 4600, Plus),
      Block("1", 5000, 6000, Plus)
    ))
    val chr1_500_1000_1500_1600_3500_3600_5500_6500_plus = BlockSet(List(
      Block("1", 500, 1000, Plus),
      Block("1", 1500, 1600, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 5500, 6500, Plus)
    ))
    val chr1_500_1000_1500_1600_plus = BlockSet(List(
      Block("1", 500, 1000, Plus),
      Block("1", 1500, 1600, Plus)
    ))
    val chr1_500_1000_1500_5500_plus = BlockSet(List(
      Block("1", 500, 1000, Plus),
      Block("1", 1500, 5500, Plus)
    ))
    val chr1_500_1000_1500_7000_plus = BlockSet(List(
      Block("1", 500, 1000, Plus),
      Block("1", 1500, 7000, Plus)
    ))
    val chr1_500_1000_2500_2600_6000_6500_8000_9000_both = BlockSet(List(
      Block("1", 500, 1000, Both),
      Block("1", 2500, 2600, Both),
      Block("1", 6000, 6500, Both),
      Block("1", 8000, 9000, Both)
    ))
    val chr1_500_1000_3500_3600_5100_5200_5300_5400_6500_6600_8500_8600_plus = BlockSet(List(
      Block("1", 500, 1000, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 5100, 5200, Plus),
      Block("1", 5300, 5400, Plus),
      Block("1", 6500, 6600, Plus),
      Block("1", 8500, 8600, Plus)
    ))
    val chr1_500_1000_5000_7000_8000_9000_plus = BlockSet(List(
      Block("1", 500, 1000, Plus),
      Block("1", 5000, 7000, Plus),
      Block("1", 8000, 9000, Plus)
    ))
    val chr1_500_1500_2000_3500_4500_5500_plus = BlockSet(List(
      Block("1", 500, 1500, Plus),
      Block("1", 2000, 3500, Plus),
      Block("1", 4500, 5500, Plus)
    ))
    val chr1_500_1500_2500_2600_3500_3600_plus = BlockSet(List(
      Block("1", 500, 1500, Plus),
      Block("1", 2500, 2600, Plus),
      Block("1", 3500, 3600, Plus)
    ))
    val chr1_500_1500_2500_3500_4500_5500_6000_7000_plus = BlockSet(List(
      Block("1", 500, 1500, Plus),
      Block("1", 2500, 3500, Plus),
      Block("1", 4500, 5500, Plus),
      Block("1", 6000, 7000, Plus)
    ))
    val chr1_500_1500_2500_3500_4500_5500_6500_6600_plus = BlockSet(List(
      Block("1", 500, 1500, Plus),
      Block("1", 2500, 3500, Plus),
      Block("1", 4500, 5500, Plus),
      Block("1", 6500, 6600, Plus)
    ))
    val chr1_500_1500_2500_3500_4500_5500_plus = BlockSet(List(
      Block("1", 500, 1500, Plus),
      Block("1", 2500, 3500, Plus),
      Block("1", 4500, 5500, Plus)
    ))
    val chr1_500_1500_3500_3600_7000_8000_plus = BlockSet(List(
      Block("1", 500, 1500, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_500_1500_3500_4500_plus = BlockSet(List(
      Block("1", 500, 1500, Plus),
      Block("1", 3500, 4500, Plus)
    ))
    val chr1_500_3000_3500_3600_7000_8000_plus = BlockSet(List(
      Block("1", 500, 3000, Plus),
      Block("1", 3500, 3600, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_500_3500_4500_4600_6500_6600_plus = BlockSet(List(
      Block("1", 500, 3500, Plus),
      Block("1", 4500, 4600, Plus),
      Block("1", 6500, 6600, Plus)
    ))
    val chr1_500_4000_4500_4600_6500_6600_plus = BlockSet(List(
      Block("1", 500, 4000, Plus),
      Block("1", 4500, 4600, Plus),
      Block("1", 6500, 6600, Plus)
    ))
    val chr1_500_4500_5100_5200_5300_5400_plus = BlockSet(List(
      Block("1", 500, 4500, Plus),
      Block("1", 5100, 5200, Plus),
      Block("1", 5300, 5400, Plus)
    ))
    val chr1_500_4500_7000_8000_plus = BlockSet(List(
      Block("1", 500, 4500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_500_5000_5100_5200_5300_5400_plus = BlockSet(List(
      Block("1", 500, 5000, Plus),
      Block("1", 5100, 5200, Plus),
      Block("1", 5300, 5400, Plus)
    ))
    val chr1_500_5000_7000_8000_plus = BlockSet(List(
      Block("1", 500, 5000, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_500_5500_6500_6600_plus = BlockSet(List(
      Block("1", 500, 5500, Plus),
      Block("1", 6500, 6600, Plus)
    ))
    val chr1_500_5500_6500_7000_plus = BlockSet(List(
      Block("1", 500, 5500, Plus),
      Block("1", 6500, 7000, Plus)
    ))
    val chr1_500_600_1000_2000_both = BlockSet(List(
      Block("1", 500, 600, Both),
      Block("1", 1000, 2000, Both)
    ))
    val chr1_500_600_1000_2000_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 1000, 2000, Plus)
    ))
    val chr1_500_600_1000_7000_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 1000, 7000, Plus)
    ))
    val chr1_500_600_1500_1600_2500_2600_4500_4600_5000_6000_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 1500, 1600, Plus),
      Block("1", 2500, 2600, Plus),
      Block("1", 4500, 4600, Plus),
      Block("1", 5000, 6000, Plus)
    ))
    val chr1_500_600_1500_1600_3500_4500_5500_6500_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 1500, 1600, Plus),
      Block("1", 3500, 4500, Plus),
      Block("1", 5500, 6500, Plus)
    ))
    val chr1_500_600_1500_1600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 1500, 1600, Plus)
    ))
    val chr1_500_600_1500_5500_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 1500, 5500, Plus)
    ))
    val chr1_500_600_2000_3000_3500_3600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 2000, 3000, Plus),
      Block("1", 3500, 3600, Plus)
    ))
    val chr1_500_600_2000_5000_7000_8000_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 2000, 5000, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_500_600_2000_9000_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 2000, 9000, Plus)
    ))
    val chr1_500_600_2500_2600_3500_3600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 2500, 2600, Plus),
      Block("1", 3500, 3600, Plus)
    ))
    val chr1_500_600_2500_2600_5500_5600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 2500, 2600, Plus),
      Block("1", 5500, 5600, Plus)
    ))
    val chr1_500_600_2500_3000_5500_5600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 2500, 3000, Plus),
      Block("1", 5500, 5600, Plus)
    ))
    val chr1_500_600_3000_4000_6500_6600_7000_8000_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 6500, 6600, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_500_600_3500_3600_both = BlockSet(List(
      Block("1", 500, 600, Both),
      Block("1", 3500, 3600, Both)
    ))
    val chr1_500_600_3500_4000_7000_8000_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 3500, 4000, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_500_600_3500_9000_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 3500, 9000, Plus)
    ))
    val chr1_500_600_4000_6000_7000_8000_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 4000, 6000, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_500_600_700_1000_1500_1600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 700, 1000, Plus),
      Block("1", 1500, 1600, Plus)
    ))
    val chr1_500_600_700_1000_3500_3600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 700, 1000, Plus),
      Block("1", 3500, 3600, Plus)
    ))
    val chr1_500_600_700_1000_5500_5600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 700, 1000, Plus),
      Block("1", 5500, 5600, Plus)
    ))
    val chr1_500_600_700_800_1000_2000_both = BlockSet(List(
      Block("1", 500, 600, Both),
      Block("1", 700, 800, Both),
      Block("1", 1000, 2000, Both)
    ))
    val chr1_500_600_700_800_1500_1600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 700, 800, Plus),
      Block("1", 1500, 1600, Plus)
    ))
    val chr1_500_600_700_800_3500_3600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 700, 800, Plus),
      Block("1", 3500, 3600, Plus)
    ))
    val chr1_500_600_700_800_5500_5600_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 700, 800, Plus),
      Block("1", 5500, 5600, Plus)
    ))
    val chr1_500_600_700_800_900_1100_both = BlockSet(List(
      Block("1", 500, 600, Both),
      Block("1", 700, 800, Both),
      Block("1", 900, 1100, Both)
    ))
    val chr1_500_600_700_800_900_2000_both = BlockSet(List(
      Block("1", 500, 600, Both),
      Block("1", 700, 800, Both),
      Block("1", 900, 2000, Both)
    ))
    val chr1_500_600_900_1100_2500_2600_3500_4000_6500_6600_7500_8500_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 900, 1100, Plus),
      Block("1", 2500, 2600, Plus),
      Block("1", 3500, 4000, Plus),
      Block("1", 6500, 6600, Plus),
      Block("1", 7500, 8500, Plus)
    ))
    val chr1_500_600_900_1100_2500_2600_3500_4500_6500_6600_7500_8500_plus = BlockSet(List(
      Block("1", 500, 600, Plus),
      Block("1", 900, 1100, Plus),
      Block("1", 2500, 2600, Plus),
      Block("1", 3500, 4500, Plus),
      Block("1", 6500, 6600, Plus),
      Block("1", 7500, 8500, Plus)
    ))
    val chr1_500_600_900_1100_both = BlockSet(List(
      Block("1", 500, 600, Both),
      Block("1", 900, 1100, Both)
    ))
    val chr1_500_600_900_2000_both = BlockSet(List(
      Block("1", 500, 600, Both),
      Block("1", 900, 2000, Both)
    ))
    val chr1_500_6500_7000_8000_plus = BlockSet(List(
      Block("1", 500, 6500, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_5500_5600_6000_6600_7000_8000_plus = BlockSet(List(
      Block("1", 5500, 5600, Plus),
      Block("1", 6000, 6600, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_5500_5600_6500_6600_7000_8000_plus = BlockSet(List(
      Block("1", 5500, 5600, Plus),
      Block("1", 6500, 6600, Plus),
      Block("1", 7000, 8000, Plus)
    ))
    val chr1_6000_7000_8000_9000_plus = BlockSet(List(
      Block("1", 6000, 7000, Plus),
      Block("1", 8000, 9000, Plus)
    ))
    val chr1_6000_8000_9000_10000_plus = BlockSet(List(
      Block("1", 6000, 8000, Plus),
      Block("1", 9000, 10000, Plus)
    ))
    val chr1_7000_8000_9000_10000_minus = BlockSet(List(
      Block("1", 7000, 8000, Minus),
      Block("1", 9000, 10000, Minus)
    ))
    val chr1_7000_8000_9000_10000_plus = BlockSet(List(
      Block("1", 7000, 8000, Plus),
      Block("1", 9000, 10000, Plus)
    ))
    val chr1_900_1000_4000_5000_6000_7000_8000_9000_both = BlockSet(List(
      Block("1", 900, 1000, Both),
      Block("1", 4000, 5000, Both),
      Block("1", 6000, 7000, Both),
      Block("1", 8000, 9000, Both)
    ))
    val chr1_900_1100_1900_2100_2900_4100_plus = BlockSet(List(
      Block("1", 900, 1100, Plus),
      Block("1", 1900, 2100, Plus),
      Block("1", 2900, 4100, Plus)
    ))
    val chr1_900_1100_1900_2100_2900_6000_plus = BlockSet(List(
      Block("1", 900, 1100, Plus),
      Block("1", 1900, 2100, Plus),
      Block("1", 2900, 6000, Plus)
    ))
    val chr2_900_1100_1900_2100_2900_6000_plus = BlockSet(List(
      Block("2", 900, 1100, Plus),
      Block("2", 1900, 2100, Plus),
      Block("2", 2900, 6000, Plus)
    ))
    val chr1_1000_1100_1200_1300_3200_3300_3400_4000_5500_6000_plus = BlockSet(List(
      Block("1", 1000, 1100, Plus),
      Block("1", 1200, 1300, Plus),
      Block("1", 3200, 3300, Plus),
      Block("1", 3400, 4000, Plus),
      Block("1", 5500, 6000, Plus)
    ))
  val chr1_1000_2000_3000_4000_5000_6000_minus = BlockSet(List(
    Block("1", 1000, 2000, Minus),
    Block("1", 3000, 4000, Minus),
    Block("1", 5000, 6000, Minus)
  ))
  val chr2_1000_2000_3000_4000_5000_6000_plus = BlockSet(List(
    Block("2", 1000, 2000, Plus),
    Block("2", 3000, 4000, Plus),
    Block("2", 5000, 6000, Plus)
  ))
  val chr2_1000_2000_3000_4000_5000_6000_minus = BlockSet(List(
    Block("2", 1000, 2000, Minus),
    Block("2", 3000, 4000, Minus),
    Block("2", 5000, 6000, Minus)
  ))

}
