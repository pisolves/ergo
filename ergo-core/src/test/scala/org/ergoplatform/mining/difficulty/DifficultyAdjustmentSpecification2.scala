package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.ChainSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration._
import scala.util.Try

class DifficultyAdjustmentSpecification2 extends ErgoCorePropertyTest {
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

  property("heightsForNextRecalculation vectors") {
    val epochLength = 128
    control.heightsForNextRecalculation(926976, epochLength) shouldBe Seq(926464, 926592, 926720, 926848, 926976)
    control.heightsForNextRecalculation(926977, epochLength) shouldBe Seq(926592, 926720, 926848, 926976, 927104)
    control.heightsForNextRecalculation(926975, epochLength) shouldBe Seq(926464, 926592, 926720, 926848, 926976)
    control.heightsForNextRecalculation(926950, epochLength) shouldBe Seq(926464, 926592, 926720, 926848, 926976)
    control.heightsForNextRecalculation(1, epochLength) shouldBe Seq(0, 128)
    control.heightsForNextRecalculation(129, epochLength) shouldBe Seq(0, 128, 256)
    control.heightsForNextRecalculation(256, epochLength) shouldBe Seq(0, 128, 256)
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
