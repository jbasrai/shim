package simulation.game

import simulation.entities.{Board, Deck, Fascist, Player}

/**
  * Created by jbasrai on 4/13/17.
  */
case class PolicyEnaction(deck: Deck,
                          board: Board,
                          players: Map[Int, Player],
                          logs: List[Log],
                          presidentId: Int,
                          chancellorId: Int,
                          votes: Map[Int, Boolean]) extends Game {

  override def step: Game = {
    val pHand = deck.draw
    val pDiscard = players(presidentId).strategy.presidentDiscardStrategy(this, pHand)

    val cHand = pHand - pDiscard
    val cDiscard = players(chancellorId).strategy.chancellorDiscardStrategy(this, cHand)

    val policy = (cHand - cDiscard).card

    val nextBoard = board.enactPolicy(policy)
    val nextLogs = Log(nextBoard, presidentId, chancellorId, votes, Some(policy)) :: logs

    if (nextBoard.liberals == 5 || nextBoard.fascists == 6) {
      GameOver(players, nextLogs)
    } else {
      val nextDeck = deck.next(nextBoard)
      val nextPresidentId = if (presidentId == 5) 1 else presidentId + 1

      Nomination(nextDeck, nextBoard, players, nextLogs, nextPresidentId)
    }
  }
}
