package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.ChainSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration._
import scala.util.Try

class DifficultyAdjustmentSpecification9 extends ErgoCorePropertyTest {
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

  property("calculate() should decrease difficulty if block time interval is higher than expected") {
    forAll(Gen.choose(UseLastEpochs, 10 * UseLastEpochs), defaultHeaderGen) { (startEpoch: Int, header: Header) =>
      whenever(header.requiredDifficulty > 10) {
        println(s"\n\ninput startEpoch(height) ${startEpoch}, updated startEpoch ${startEpoch * Epoch + 1}")
        println(s"Header nBits ${header.nBits}")
        val previousHeaders = control.previousHeightsRequiredForRecalculation(startEpoch * Epoch + 1, Epoch)
          .map { i => 
            val updatedHeader = header.copy(timestamp = header.timestamp + DesiredInterval.toMillis * 2 * i, height = i)
            println(s"Header Timestamp ${header.timestamp}")
            println(s"Updated Height: $i, DesiredInterval: ${DesiredInterval.toMillis}, Timestamp: ${header.timestamp + DesiredInterval.toMillis * 2 *i}")
            updatedHeader
          }
        previousHeaders.length shouldBe UseLastEpochs + 1

        control.calculate(previousHeaders, Epoch) < header.requiredDifficulty shouldBe true
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
