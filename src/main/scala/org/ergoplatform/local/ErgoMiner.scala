package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, Cancellable}
import org.ergoplatform.local.ErgoMiner.{ProduceCandidate, MineBlock, StartMining, StopMining}
import org.ergoplatform.modifiers.history.{CandidateBlock, Header}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.utils.ScorexLogging


class ErgoMiner(ergoSettings: ErgoSettings, viewHolder: ActorRef) extends Actor with ScorexLogging {

  private var cancellableOpt: Option[Cancellable] = None
  private var mining = false
  private var startingNonce = Long.MinValue

  private val powScheme = ergoSettings.chainSettings.poWScheme

  override def preStart(): Unit = {
  }

  override def receive: Receive = {
    case StartMining =>
      log.info("Starting Mining")
      self ! ProduceCandidate

    case ProduceCandidate =>
      viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, CandidateBlock] { v =>
        val txs = v.state.filterValid(v.pool.take(1000).toSeq)
        val (adProof, adDigest) = v.state.proofsForTransactions(txs).get

        val timestamp = System.currentTimeMillis()
        val votes = Array.fill(5)(0:Byte)
        CandidateBlock(v.history.bestHeaderOpt, Constants.InitialNBits, adDigest, adProof, txs, timestamp, votes)
      }

    case candidate: CandidateBlock =>
      startingNonce = Long.MinValue
      self ! MineBlock(candidate)

    case StopMining =>

    case MineBlock(candidate) =>
      val start = startingNonce
      val finish = start + 10
      startingNonce = finish

      log.info(s"Trying nonces from $start till $finish")

      ergoSettings.chainSettings.poWScheme.proveBlock(
        candidate,
        start,
        finish
      ) match {
        case Some(newBlock) =>
          log.info("New block found: " + newBlock)
          viewHolder ! LocallyGeneratedModifier[Header](newBlock.header)
          self ! ProduceCandidate
        case None =>
          self ! MineBlock(candidate)
      }
  }
}


object ErgoMiner {

  case object StartMining

  case object ProduceCandidate

  case object StopMining

  case class MineBlock(candidate: CandidateBlock)
}