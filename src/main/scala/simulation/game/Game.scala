package simulation.game

import simulation.entities._
import simulation.strategy.{DefaultLiberalStrategy, SimpleFascistStrategy}

import scala.util.Random

/**
  * Created by jbasrai on 4/13/17.
  */
trait Game {
  def step: Game
}

object Game {
  val players = {
    val fascists = List.fill(11)(Player(Fascist, SimpleFascistStrategy))
    val liberals = List.fill(6)(Player(Liberal, DefaultLiberalStrategy))

    val everyone = fascists ::: liberals

    val shuffled = Random.shuffle(everyone)

    shuffled.zipWithIndex.map{ case (player, index) => (index + 1, player) }.toMap
  }

  def reset = {
    Nomination(Deck.reset, Board.reset, players, Nil, 1)
  }
}
