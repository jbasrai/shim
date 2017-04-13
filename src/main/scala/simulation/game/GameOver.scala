package simulation.game

import simulation.entities.{Fascist, Liberal, Player}

/**
  * Created by jbasrai on 4/13/17.
  */
case class GameOver(players: Map[Int, Player], logs: List[Log]) extends Game {
  override def step: Game = GameOver(players, logs)

  val lastTurn = logs.head

  val winner = if(lastTurn.board.fascists == 6) Fascist else Liberal
}
