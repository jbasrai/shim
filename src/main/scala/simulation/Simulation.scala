package simulation

import entities._
import simulation.game.{Game, GameOver}

/**
  * Created by jbasrai on 4/10/17.
  */
class Simulation {
  def run(trials: Int) = {
    def loop(results: List[GameOver], t: Int): List[GameOver] =
      if (t > trials) results
      else loop(simulateGame :: results, t + 1)

    loop(Nil, 1)
  }

  def simulateGame: GameOver = {
    def loop(game: Game): GameOver = game match {
      case game: GameOver => game
      case _ => loop(game.step)
    }

    loop(Game.reset)
  }
}

object Simulation {
  def main(args: Array[String]): Unit = {
    val results: List[GameOver] = (new Simulation).run(100)
    println(results.count(_.winner == Liberal))
  }
}
