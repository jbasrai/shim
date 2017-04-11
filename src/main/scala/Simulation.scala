import scala.util.Random

/**
  * Created by jbasrai on 4/10/17.
  */
class Simulation(numberOfGames: Int) {
  // main entry point of the project
  // runs the simulation from start to finish, over <numberOfGames> trials
  // returns a reverse-ordered list of final game states, each also containing the complete game log
  def start: List[Game] = runSimulation(1, Nil)

  private def runSimulation(gameCount: Int, results: List[Game]): List[Game] = {
    if (gameCount > numberOfGames)
      results
    else
      runSimulation(gameCount + 1, simulateGame(Game.reset) :: results)
  }

  // plays out a game from start to finish, returning the final game state
  private def simulateGame(game: Game): Game = {
    if (!game.isOver) {
      val nextGameState = simulateTurn(game)
      simulateGame(nextGameState)
    }
    else
      game
  }

  // the meat of the simulation is contained here
  private def simulateTurn(game: Game): Game = {
    // the president draws 3 cards, discards a card, and passes the remaining hand to the chancellor
    val pHand = game.deck.draw
    val nextDeck = game.deck.next(game.board)

    val pDiscard = pHand match {
      case H3b => Liberal
      case _ => Fascist
    }

    // then the chancellor discards a card and enacts the remaining policy
    val cHand = pHand.discard(pDiscard)

    val cDiscard = cHand match {
      case H2b => Liberal
      case _ => Fascist
    }

    val enactedPolicy =
      if (cHand.discard(cDiscard).fascists == 1) Fascist
      else Liberal

    val nextBoard = game.board.enactPolicy(enactedPolicy)

    val nextLog = Turn(pHand, pDiscard, cDiscard, enactedPolicy) :: game.log

    Game(nextDeck, nextBoard, nextLog)
  }
}

case class Game(deck: Deck,
                board: Board,
                log: List[Turn]) {
  val isOver = board.fascists == 6 || board.liberals == 5

  val winner: Faction =
    if (board.fascists == 6)
      Fascist
    else
      Liberal
}

object Game {
  def reset: Game = Game(Deck.reset, Board.reset, Nil)
}

case class Turn(draw: Hand,
                pDiscard: Faction,
                cDiscard: Faction,
                enactedPolicy: Faction)

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
    if (policies.length >= 6)
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

// this is not a List[Policy] in order to relax equality and allow pattern matching
// for now, assume order of the cards in hand does not matter
case class Hand(fascists: Int, liberals: Int) {
  // does not protect against invalid hands, discard responsibly
  def discard(faction: Faction): Hand = faction match {
    case Fascist => Hand(fascists - 1, liberals)
    case Liberal => Hand(fascists, liberals - 1)
  }
}

object Hand {
  // helps simply construction when hand is drawn from deck: List[Policy]
  def fromList(policies: List[Policy]): Hand =
    Hand(policies count (_ == Policy(Fascist)),
      policies count (_ == Policy(Liberal)))
}

// more expressive names
object H3r extends Hand(3, 0)
object H2r1b extends Hand(2, 1)
object H1r2b extends Hand(1, 2)
object H3b extends Hand(0, 3)
object H2r extends Hand(2, 0)
object H1r1b extends Hand(1, 1)
object H2b extends Hand(0, 2)

// really the only time we care about this is for the deck, using faction by itself is usually simpler
case class Policy(faction: Faction)

sealed trait Faction
case object Fascist extends Faction
case object Liberal extends Faction
