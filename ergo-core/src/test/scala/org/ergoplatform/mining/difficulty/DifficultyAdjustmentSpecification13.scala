package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.ChainSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration._
import scala.util.Try

class DifficultyAdjustmentSpecification13 extends ErgoCorePropertyTest {
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

  property("calculate() for different epoch lengths and constant hashrate") {
    forAll(defaultHeaderGen, smallPositiveInt, smallPositiveInt, Gen.choose(1, 60 * 60 * 1000)) { (header: Header, epoch, useLastEpochs, interval) =>
      whenever(useLastEpochs > 1 && header.requiredDifficulty >= 1) {
        val control = new DifficultyAdjustment(chainSettingsMod(interval.millis, useLastEpochs, epoch))
        val previousHeaders = control.previousHeightsRequiredForRecalculation(epoch * useLastEpochs + 1, epoch)
          .map(i => header.copy(timestamp = header.timestamp + i * interval, height = i))
        previousHeaders.length shouldBe useLastEpochs + 1
        control.calculate(previousHeaders, epoch) shouldBe header.requiredDifficulty
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
