package simulation.game

import simulation.entities.{Board, Deck, Player}

/**
  * Created by jbasrai on 4/13/17.
  */
case class Nomination(deck: Deck,
                      board: Board,
                      players: Map[Int, Player],
                      logs: List[Log],
                      presidentId: Int) extends Game {

  override def step: Game = {
    val nominationMap = Map(
      1 -> 5,
      2 -> 4,
      3 -> 1,
      4 -> 2,
      5 -> 1
    )

    Election(deck, board, players, logs, presidentId, nominationMap(presidentId))
  }
}
