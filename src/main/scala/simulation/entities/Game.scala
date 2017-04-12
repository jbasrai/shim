package simulation.entities

import simulation._
import simulation.strategy.Strategy

/**
  * Created by jbasrai on 4/11/17.
  */
case class Game(deck: Deck,
                board: Board,
                players: PlayerGroup,
                log: List[Turn]) {
  val isOver = board.fascists == 6 || board.liberals == 5

  val winner: Faction =
    if (board.fascists == 6)
      Fascist
    else
      Liberal
}

object Game {
  def reset(strategies: Map[Faction, Strategy]): Game =
    Game(Deck.reset, Board.reset, PlayerGroup.reset(strategies), Nil)
}

