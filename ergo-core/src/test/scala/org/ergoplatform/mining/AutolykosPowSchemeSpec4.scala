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

class AutolykosPowSchemeSpec4 extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  // testing an invalid header got from a mining pool
  property("test vector - invalid solution") {
    import io.circe.parser._

    val headerJson = "{\"extensionId\":\"277907e4e5e42f27e928e6101cc4fec173bee5d7728794b73d7448c339c380e5\",\"difficulty\":\"1325481984\",\"votes\":\"000000\",\"timestamp\":1611225263165,\"size\":219,\"stateRoot\":\"c0d0b5eafd07b22487dac66628669c42a242b90bef3e1fcdc76d83140d58b6bc0e\",\"height\":2870,\"nBits\":72286528,\"version\":2,\"id\":\"5b0ce6711de6b926f60b67040cc4512804517785df375d063f1bf1d75588af3a\",\"adProofsRoot\":\"49453875a43035c7640dee2f905efe06128b00d41acd2c8df13691576d4fd85c\",\"transactionsRoot\":\"770cbb6e18673ed025d386487f15d3252115d9a6f6c9b947cf3d04731dd6ab75\",\"extensionHash\":\"9bc7d54583c5d44bb62a7be0473cd78d601822a626afc13b636f2cbff0d87faf\",\"powSolutions\":{\"pk\":\"0288114b0586efea9f86e4587f2071bc1c85fb77e15eba96b2769733e0daf57903\",\"w\":\"0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798\",\"n\":\"000100000580a91b\",\"d\":0},\"adProofsId\":\"4fc36d59bf26a672e01fbfde1445bd66f50e0f540f24102e1e27d0be1a99dfbf\",\"transactionsId\":\"d196ef8a7ef582ab1fdab4ef807715183705301c6ae2ff0dcbe8f1d577ba081f\",\"parentId\":\"ab19e6c7a4062979dddb534df83f236d1b949c7cef18bcf434a67e87c593eef9\"}"

    val json = parse(headerJson).toOption.get

    val header = Header.jsonDecoder.decodeJson(json).toOption.get

    val pow = new AutolykosPowScheme(32, 26)

    pow.validate(header).isSuccess shouldBe false

    val result = pow.validate(header).isSuccess

    println(s"\ninvalid header: $header")
    println(s"result = $result")
  }
}
