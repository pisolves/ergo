package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.ChainSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration._
import scala.util.Try

class DifficultyAdjustmentSpecification10 extends ErgoCorePropertyTest {
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

  property("interpolate() vectors") {
    val diff = BigInt("675204474840679645414180963439886534428")
    control.interpolate(Seq((799167010, diff), (799167133, diff), (799167256, diff), (799167379, diff)), Epoch) shouldBe diff

    control.interpolate(Seq((123, diff), (246, diff), (369, diff), (492, diff)), Epoch) shouldBe diff

    control.interpolate(Vector((123, diff), (246, diff * 2), (369, diff * 2), (492, diff)), Epoch) shouldBe (diff * 3 / 2)

    control.interpolate(Vector((123, diff), (246, diff * 2), (369, diff * 3), (492, diff * 4)), Epoch) shouldBe BigInt("3376022374203398227070904817199432672139")

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
