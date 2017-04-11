import scala.util.Random

/**
  * Created by jbasrai on 4/10/17.
  */
class Simulation(numberOfGames: Int) {
  def start: List[Game] = runSimulation(1, Nil)

  def runSimulation(gameCount: Int, results: List[Game]): List[Game] = {
    if (gameCount > numberOfGames) {
      results
    } else {
      runSimulation(gameCount + 1, simulateGame(Game.reset) :: results)
    }
  }

  def simulateGame(game: Game): Game = {
    if (!game.isOver) {
      val next = simulateTurn(game)
      simulateGame(next)
    } else {
      game
    }
  }

  def simulateTurn(game: Game): Game = {
    Game(game.deck.next(game.board), game.deck.draw match {
      case H3r => {
        game.board.enactPolicy(Fascist)
      }
      case _ => {
        game.board.enactPolicy(Liberal)
      }
    })
  }
}

case class Game(deck: Deck,
                board: Board) {
  val isOver = board.fascists == 6 || board.liberals == 5

  val winner: Faction =
    if (board.fascists == 6)
      Fascist
    else
      Liberal
}

object Game {
  def reset: Game = Game(Deck.reset, Board.reset)
}

case class Board(fascists: Int, liberals: Int) {
  def enactPolicy(faction: Faction): Board = faction match {
    case Fascist => Board(fascists + 1, liberals)
    case Liberal => Board(fascists, liberals + 1)
  }
}

object Board {
  def reset: Board = Board(0, 0)
}

case class Deck(policies: List[Policy]) {
  val draw: Hand = Hand.fromList(policies take 3)

  def next(board: Board): Deck = {
    if (policies.length >= 3)
      Deck(policies drop 3)
    else
      Deck.shuffle(
        Deck.totalFascistPolicies - board.fascists,
        Deck.totalLiberalPolicies - board.liberals)
  }
}

object Deck {
  val totalFascistPolicies = 11
  val totalLiberalPolicies = 6

  def shuffle(numberOfFascistPolicies: Int, numberOfLiberalPolicies: Int): Deck = {
    val fascistPolicies = List.fill(numberOfFascistPolicies)(Policy(Fascist))
    val liberalPolicies = List.fill(numberOfLiberalPolicies)(Policy(Liberal))

    val allPolicies = fascistPolicies ::: liberalPolicies

    val shuffled = Random.shuffle(allPolicies)
    Deck(shuffled)
  }

  def reset: Deck = shuffle(totalFascistPolicies, totalLiberalPolicies)
}

case class Hand(fascists: Int, liberals: Int)
object H3r extends Hand(3, 0)
object H2r1b extends Hand(2, 1)
object H1r2b extends Hand(1, 2)
object H3b extends Hand(0, 3)

object Hand {
  def fromList(policies: List[Policy]): Hand =
    Hand(policies count (_ == Policy(Fascist)),
      policies count (_ == Policy(Liberal)))
}

case class Policy(faction: Faction)

sealed trait Faction
case object Fascist extends Faction
case object Liberal extends Faction
