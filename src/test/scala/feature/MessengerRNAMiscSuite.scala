package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class MessengerRNAMiscSuite extends FunSuite {

  test("CDS start and end") {

    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 99, 150, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 140, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 300, 401, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 250, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 401, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 50, 60, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 450, 460, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 150, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 250, 260, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 299, 310, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 201, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 200, 350, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 200, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 300, Some("name"), Some("gene")) }

    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 99, 150, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 140, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 300, 401, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 250, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 401, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 50, 60, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 450, 460, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 150, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 250, 260, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 299, 310, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 201, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 200, 350, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 200, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 300, Some("name"), Some("gene")) }

  }

}
