package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.ChainSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration._
import scala.util.Try

class DifficultyAdjustmentSpecification14 extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  val precision = 0.0001
  val minDiff: BigInt = (BigDecimal(1) / precision).toBigInt
  val Epoch = 123

  val UseLastEpochs = 4
  val DesiredInterval: FiniteDuration = 1.minute

  def chainSettingsMod(blockInterval: FiniteDuration = DesiredInterval,
                    useLastEpochs: Int = UseLastEpochs,
                    epochLength: Int = Epoch): ChainSettings =
    chainSettings.copy(blockInterval = blockInterval, useLastEpochs = useLastEpochs, epochLength = epochLength)

  val control = new DifficultyAdjustment(chainSettingsMod())

  property("calculate() for different epoch lengths and linear hashrate") {
    val step = 1000
    forAll(defaultHeaderGen, smallPositiveInt, smallPositiveInt, Gen.choose(1, 60 * 60 * 1000)) { (header: Header, epoch, useLastEpochs, interval) =>
      whenever(useLastEpochs > 1) {
        println(s"input Header nBits: ${header.nBits}")
        println(s"input Header timestamp: ${header.timestamp}, height: ${header.height}")
        println(s"input chainsetting useLastEpochs: $useLastEpochs")
        println(s"Input chainsetting epoch: $epoch")
        println(s"Input chaingsetting interval: ${interval.millis}")
        val control = new DifficultyAdjustment(chainSettingsMod(interval.millis, useLastEpochs, epoch))
        val previousHeaders = control.previousHeightsRequiredForRecalculation(epoch * useLastEpochs + 1, epoch).map { i =>
          header.copy(timestamp = header.timestamp + i * interval,
            height = i,
            nBits = DifficultySerializer.encodeCompactBits(DifficultySerializer.decodeCompactBits(header.nBits) + step))
        }

        previousHeaders.length shouldBe useLastEpochs + 1
        val expectedDifficulty = previousHeaders.last.requiredDifficulty + step
        val error = BigDecimal(control.calculate(previousHeaders, epoch) - expectedDifficulty) / BigDecimal(expectedDifficulty)
        println(s"expectedDifficulty:$expectedDifficulty")
        println(s"error:$error")
        val err = error should be < BigDecimal(1) / DifficultyAdjustment.PrecisionConstant
      }
    }
  }

  def equalsWithPrecision(i: BigInt, j: BigInt): Unit = {
    require((BigDecimal(i - j) / BigDecimal(j)).toDouble < precision, s"$i and $j are too different")
  }

  def diffGen: Gen[BigInt] = for {
    rnd <- Arbitrary.arbitrary[BigInt]
    diff = if (rnd < 0) -rnd + minDiff else rnd + minDiff
    _ = assert(diff > 0, s"$diff,$minDiff,$rnd")
  } yield diff


  def epochGen: Gen[Int] = Gen.choose(1, Int.MaxValue - UseLastEpochs)

}
