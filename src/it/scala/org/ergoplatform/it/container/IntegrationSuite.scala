package org.ergoplatform.it.container

import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, Suite}
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContext
import scala.util.Random

trait IntegrationSuite
  extends BeforeAndAfterAll
    with IntegrationTestConstants
    with ErgoTestHelpers
    with ScalaFutures
    with IntegrationPatience
    with Matchers
    with ScorexLogging { this: Suite =>

  implicit def executionContext: ExecutionContext = ErgoTestHelpers.defaultExecutionContext

  val tempDir: String = System.getenv("TMPDIR")

  protected val localDataDir: String = s"$tempDir/ergo-${Random.nextInt(Int.MaxValue)}"

  protected val docker: Docker = new Docker(tag = getClass.getSimpleName, localDataVolumeOpt = Some(localDataDir))

  override protected def beforeAll(): Unit = {
    log.debug("Starting tests")
  }

  override protected def afterAll(): Unit = docker.close()

}
