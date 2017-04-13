package simulation.game

import simulation.entities.{Board, Deck, Player}

/**
  * Created by jbasrai on 4/13/17.
  */
case class Election(deck: Deck,
                    board: Board,
                    players: Map[Int, Player],
                    logs: List[Log],
                    presidentId: Int,
                    chancellorId: Int) extends Game {

  override def step: Game = {
    val votes = List(1,2,3,4,5).map((_, true)).toMap

    PolicyEnaction(deck, board, players, logs, presidentId, chancellorId, votes)
  }
}
