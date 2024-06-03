package org.ergoplatform.mining

import com.google.common.primitives.Ints
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import cats.syntax.either._
import sigmastate.crypto.CryptoConstants.EcPointType
import io.circe.parser.decode

class AutolykosPowSchemeSpec2 extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  property("calcN test vectors") {
    // mainnet parameters
    val k = 32
    val n = 26

    val pow = new AutolykosPowScheme(k, n)

    // N is always the same in Autolykos v1
    pow.calcN(1, 700000) shouldBe pow.NBase
    var exp_result = pow.NBase
    var result = pow.calcN(1, 700000)
    println(s"1st Result : $result, $exp_result")

    pow.calcN(1, 100000) shouldBe pow.NBase
    exp_result = pow.NBase
    result = pow.calcN(1, 100000)
    println(s"2nd Result : $result, $exp_result")

    pow.calcN(1, 70000000) shouldBe pow.NBase
    exp_result = pow.NBase
    result = pow.calcN(1, 70000000)
    println(s"3rd Result : $result, $exp_result")

    pow.calcN(2, 500000) shouldBe pow.NBase
    exp_result = pow.NBase
    result = pow.calcN(2, 500000)
    println(s"4th Result : $result, $exp_result")

    pow.calcN(2, 600000) shouldBe pow.NBase
    exp_result = pow.NBase
    result = pow.calcN(2, 600000)
    println(s"5th Result : $result, $exp_result")

    pow.calcN(2, 600 * 1024) shouldBe 70464240
    exp_result = 70464240
    result = pow.calcN(2, 600 * 1024)
    println(s"6th Result : $result, $exp_result")

    pow.calcN(2, 650 * 1024) shouldBe 73987410
    exp_result = 73987410
    result = pow.calcN(2, 650 * 1024)
    println(s"7th Result : $result, $exp_result")

    pow.calcN(2, 700000) shouldBe 73987410
    exp_result = 73987410
    result = pow.calcN(2, 700000)
    println(s"8th Result : $result, $exp_result")

    pow.calcN(2, 788400) shouldBe 81571035 // 3 years
    exp_result = 81571035
    result = pow.calcN(2, 788400)
    println(s"9th Result : $result, $exp_result")

    pow.calcN(2, 1051200) shouldBe 104107290 // 4 years
    exp_result = 104107290
    result = pow.calcN(2, 1051200)
    println(s"10th Result : $result, $exp_result")

    pow.calcN(2, 4198400) shouldBe 2143944600 // max height
    exp_result = 2143944600
    result = pow.calcN(2, 4198400)
    println(s"11st Result : $result, $exp_result")

    pow.calcN(2, 41984000) shouldBe 2143944600
    exp_result = 2143944600
    result = pow.calcN(2, 41984000)
    println(s"12nd Result : $result, $exp_result")
  }
}
