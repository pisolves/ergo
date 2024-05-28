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

class AutolykosPowSchemeSpec1 extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  property("generated solution should be valid") {
    val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
    def test (inHeader: Header, difficulty: BigInt, ver: Byte) = {
      val nBits = DifficultySerializer.encodeCompactBits(difficulty)
      val h = inHeader.copy(nBits = nBits, version = ver)
      val sk = randomSecret()
      val x = randomSecret()
      val msg = pow.msgByHeader(h)
      val b = pow.getB(h.nBits)
      val hbs = Ints.toByteArray(h.height)
      val N = pow.calcN(h)
      val newHeader = pow.checkNonces(ver, hbs, msg, sk, x, b, N, 0, 1000)
      .map(s => h.copy(powSolution = s)).get
      pow.validate(newHeader) shouldBe 'success

      if(ver > Header.InitialVersion) {
        // We remove last byte of "msg", perform PoW and check that it fails validation
        require(HeaderSerializer.bytesWithoutPow(h).last == 0)
        val msg2 = Blake2b256(HeaderSerializer.bytesWithoutPow(h).dropRight(1))

        val newHeader2 = pow.checkNonces(ver, hbs, msg2, sk, x, b, N, 0, 1000)
        .map(s => h.copy(powSolution = s)).get
        pow.validate(newHeader2) shouldBe 'failure
      }
    }

    val inHeader = invalidHeaderGen.sample.get

    val inHeaderJson: String =
      """
      {
        "extensionId": "0c93b35a5d7c53aa68033b7a90a3f9461f4107ecc27f05d82d925641e98873cf",
        "difficulty": "5387479432753577984",
        "votes": "000000",
        "timestamp": 8487956500612830582,
        "size": 225,
        "stateRoot": "003b80de80ff0866ff0130008580d2c90001678001ffe58000ba190401afff80ff",
        "height": 482839614,
        "nBits": 139117612,
        "version": 124,
        "id": "7990e21f1b00803b2a57f9649f4513ef6cbae4004aa42d1688602df1a498d77a",
        "adProofsRoot": "7680007291770480011e007fad66ff7e1048c801e1ff1fffcc8060e40daf687a",
        "transactionsRoot": "fa806801b470013b80126e7f0001912bd94b01f400f960018069967f01007fff",
        "extensionHash": "547fc3b880809ad80100a6017f0188f3000fcfc57b14ffd005ffd4a62f01481b",
        "powSolutions": {
          "pk": "03673d30846dfc02a2562863e306005a6e401fabb7d81117dbb2f302a91852e76f",
          "w": "02dfe75de463263d8d98c07f76fb5668799ea2ae89a56eb6ebf58c9dbb9714e583",
          "n": "ff7f80f45118b87f",
          "d": 115792089237316195423570985008687907852837564279074904382596277580756921012439
        },
        "adProofsId": "f856a2458171d96ec259fc31a9c055b321167ce7b00f8575bc74131f1f92d5fa",
        "transactionsId": "e314f4225c7d9dd8cde01b9c03500c0859832749741bd1bb04b289a27229023e",
        "parentId": "8b807c7fa5f7017ffa007fd401b57fac7f00807bf30133017f7f800007637f00"
      }
      """

    val decodedHeader: Either[io.circe.Error, Header] = decode[Header](inHeaderJson)

    decodedHeader match {
      case Right(header) => 
        //println(s"Successfully decoded header: $header")
        val inHeader = header
        val difficulty:BigInt = 100
        val ver:Byte = 1

        test(inHeader, difficulty, ver)
      case Left(error) => 
        println(s"Failed to decode header: $error")
    }
  }
}
