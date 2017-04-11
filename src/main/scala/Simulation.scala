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
    if (!game.players.isGovtActive) {
      val nextGameState = simulateVote(game)
      simulateTurn(nextGameState)
    } else {
      // the president draws 3 cards, discards a card, and passes the remaining hand to the chancellor
      val pHand = game.deck.draw
      assert(pHand.size == 3)

      val pDiscard = pHand match {
        case H3b => Liberal
        case _ => Fascist
      }

      // then the chancellor discards a card and enacts the remaining policy
      val cHand = pHand - pDiscard
      assert(cHand.size == 2)

      val cDiscard = cHand match {
        case H2b => Liberal
        case _ => Fascist
      }

      val enactedPolicy = (cHand - cDiscard).card

      val nextBoard = game.board.enactPolicy(enactedPolicy)

      val nextLog = Turn(
        game.players.presidentId,
        game.players.chancellorId,
        pHand, pDiscard, cDiscard, enactedPolicy) :: game.log

      val nextDeck = game.deck.next(game.board)

      val nextPlayers = game.players.advancePresident

      Game(nextDeck, nextBoard, nextPlayers, nextLog)

    }
  }

  private def simulateVote(game: Game): Game = {
    val voteMap = Map(
      1 -> 5,
      2 -> 4,
      3 -> 1,
      4 -> 2,
      5 -> 1
    )

    val presidentId = game.players.presidentId
    val nextPlayers = game.players.electChancellor(voteMap(presidentId))

    game.copy(players = nextPlayers)
  }
}

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
  def reset: Game = Game(Deck.reset, Board.reset, PlayerGroup.reset, Nil)
}

case class PlayerGroup(players: Set[Player]) {
  val isGovtActive = players.exists(_.isChancellor)

  val presidentId = players.find(_.isPresident).get.id

  val chancellorId =
    if (isGovtActive) players.find(_.isChancellor).get.id
    else -1

  def advancePresident = {
    val nextPresidentId = if (presidentId == 5) 1 else presidentId + 1

    val nextPlayers = players.map(player =>
      if (player.id == presidentId) player.copy(isPresident = false)
      else if (player.id == nextPresidentId) player.copy(isPresident = true)
      else player
    ).map(_.copy(isChancellor = false))

    PlayerGroup(nextPlayers)
  }

  def electChancellor(playerId: Int): PlayerGroup = {
    val nextPlayers = players.map(player =>
      if (player.id != playerId) player
      else player.copy(isChancellor = true)
    )

    PlayerGroup(nextPlayers)
  }
}

object PlayerGroup {
  val numberOfLiberals = 3
  val numberOfFascists = 2

  def reset: PlayerGroup = {
    val liberals = List.fill(numberOfLiberals)(Player(0, Liberal, false, false, true))
    val fascists = List.fill(numberOfFascists)(Player(0, Fascist, false, false, true))

    val allPlayers = liberals ::: fascists

    val shuffled = Random.shuffle(allPlayers)

    val addPresident = shuffled.head.copy(isPresident = true) :: shuffled.tail

    val addIds = addPresident.zipWithIndex.map { case (player, index) =>
      player.copy(id = index + 1)
    }

    PlayerGroup(addIds.toSet)
  }
}

case class Player(id: Int,
                  faction: Faction,
                  isPresident: Boolean,
                  isChancellor: Boolean,
                  eligibleForChancellor: Boolean)

case class Turn(presidentId: Int,
                chancellorId: Int,
                draw: Hand,
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
  def reset = Board(0, 0)
}

case class Deck(policies: List[Policy]) {
  val draw: Hand = Hand.fromList(policies take 3)

  // side effect in form of Random.shuffle. this method should only be called once
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
  assert(fascists >= 0 && liberals >= 0)

  // does not protect against invalid hands, discard responsibly
  def discard(faction: Faction): Hand = faction match {
    case Fascist => Hand(fascists - 1, liberals)
    case Liberal => Hand(fascists, liberals - 1)
  }

  def -(faction: Faction): Hand = this discard faction

  // should only be called when size is 1
  val card = if (fascists == 1) Fascist else Liberal

  val size = fascists + liberals
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
