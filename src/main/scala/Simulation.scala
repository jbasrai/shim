import scala.util.Random

/**
  * Created by jbasrai on 4/10/17.
  */
class Simulation(strategy: Strategy, numberOfGames: Int) {
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

  // all strategy calls are contained here
  private def simulateTurn(game: Game): Game = {
    // nominate chancellor
    if (!game.players.isChancellorPicked)
      simulateTurn(simulateNomination(game))
    // vote to elect chancellor
    else if (!game.players.haveVoted)
      simulateTurn(simulateVote(game))
    // president and chancellor enact policy
    // if vote fails, go straight to clean up instead
    else if (game.players.haveVoted && !game.board.wasPolicyJustEnacted)
      simulateTurn(simulatePolicyEnaction(game))
    // saves game state to log and prepares game for next turn
    else
      cleanUp(game)
  }

  private def simulateNomination(game: Game): Game = {
    val voteMap = Map(
      1 -> 5,
      2 -> 4,
      3 -> 1,
      4 -> 2,
      5 -> 1
    )

    val presidentId = game.players.presidentId
    val nextPlayers = game.players.pickChancellor(voteMap(presidentId))

    game.copy(players = nextPlayers)
  }

  private def simulateVote(game: Game): Game = {
    val nextPlayers = game.players.identity.map(_.copy(vote = Some(true)))
    game.copy(players = PlayerGroup(nextPlayers))
  }

  private def simulatePolicyEnaction(game: Game): Game = {
    assert(game.players.presidentId > -1)
    assert(game.players.chancellorId > -1)

    // the president draws 3 cards, discards a card, and passes the remaining hand to the chancellor
    val pHand = game.deck.draw
    assert(pHand.size == 3)

    val pDiscard = strategy.presidentDiscard(game, pHand)

    // then the chancellor discards a card and enacts the remaining policy
    val cHand = pHand - pDiscard
    assert(cHand.size == 2)

    val cDiscard = strategy.chancellorDiscard(game, cHand)

    val enactedPolicy = (cHand - cDiscard).card

    val nextBoard = game.board.enactPolicy(enactedPolicy)

    // update player info
    val presidentId = game.players.presidentId
    val chancellorId = game.players.chancellorId
    val nextPlayers = game.players
      .hand(pHand, presidentId)
      .discard(pDiscard, presidentId)
      .hand(cHand, chancellorId)
      .discard(cDiscard, chancellorId)

    game.copy(board = nextBoard, players = nextPlayers)
  }

  private def cleanUp(game: Game): Game = {
    assert(game.players.presidentId > -1)
    assert(game.players.chancellorId > -1)

    val presidentId = game.players.presidentId
    val chancellorId = game.players.chancellorId

    val nextLog = Turn(
      presidentId,
      chancellorId,
      game.players.votes,
      game.players.handOf(presidentId),
      game.players.discardOf(presidentId),
      game.players.handOf(chancellorId),
      game.players.discardOf(chancellorId),
      game.board.lastEnactedPolicy) :: game.log

    val nextDeck = game.deck.next(game.board)

    val nextBoard = game.board.copy(lastEnactedPolicy = None)

    val nextPlayers = game.players.advancePresident

    Game(nextDeck, nextBoard, nextPlayers, nextLog)
  }

}

abstract class Strategy {
  private def discard(liberalStrategy: (Game, Hand) => Faction,
              fascistStrategy: (Game, Hand) => Faction,
              game: Game, hand: Hand): Faction = {

    val presidentFaction = game.players.factionOf(game.players.presidentId)

    presidentFaction match {
      case Liberal => liberalStrategy(game, hand)
      case Fascist => fascistStrategy(game, hand)
    }
  }

  def presidentDiscard(game: Game, hand: Hand): Faction =
    discard(liberalPresidentDiscard, fascistPresidentDiscard, game, hand)

  def chancellorDiscard(game: Game, hand: Hand): Faction =
    discard(liberalChancellorDiscard, fascistChancellorDiscard, game, hand)

  protected def liberalPresidentDiscard(game: Game, hand: Hand): Faction

  protected def liberalChancellorDiscard(game: Game, hand: Hand): Faction

  protected def fascistPresidentDiscard(game: Game, hand: Hand): Faction

  protected def fascistChancellorDiscard(game: Game, hand: Hand): Faction
}

// everyone plays liberal
class DumbLiberalStrategy extends Strategy {
  private def discardFascist(hand: Hand) =
    if (hand canDiscard Fascist) Fascist
    else Liberal

  override def liberalPresidentDiscard(game: Game, hand: Hand): Faction =
    discardFascist(hand)

  override def liberalChancellorDiscard(game: Game, hand: Hand): Faction =
    discardFascist(hand)

  override def fascistPresidentDiscard(game: Game, hand: Hand): Faction =
    discardFascist(hand)

  override def fascistChancellorDiscard(game: Game, hand: Hand): Faction =
    discardFascist(hand)
}

class SimpleFascistStrategy extends Strategy {
  private def discardFascist(hand: Hand) =
    if (hand canDiscard Fascist) Fascist
    else Liberal

  private def discardLiberal(hand: Hand) =
    if (hand canDiscard Liberal) Liberal
    else Fascist

  override def liberalPresidentDiscard(game: Game, hand: Hand): Faction =
    discardFascist(hand)

  override def liberalChancellorDiscard(game: Game, hand: Hand): Faction =
    discardFascist(hand)

  override def fascistPresidentDiscard(game: Game, hand: Hand): Faction =
    discardLiberal(hand)

  override def fascistChancellorDiscard(game: Game, hand: Hand): Faction =
    discardFascist(hand)
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
  val identity = players

  val isChancellorPicked = players.exists(_.isChancellor)

  val presidentId = players.find(_.isPresident).map(_.id) getOrElse -1

  val chancellorId = players.find(_.isChancellor).map(_.id) getOrElse -1

  def fromId(playerId: Int) = players.find(_.id == playerId)

  lazy val advancePresident = {
    val nextPresidentId = if (presidentId == 5) 1 else presidentId + 1

    val nextPlayers = players.map(player =>
      if (player.id == presidentId) player.copy(isPresident = false)
      else if (player.id == nextPresidentId) player.copy(isPresident = true)
      else player
    ).map(_.copy(isChancellor = false))

    PlayerGroup(nextPlayers)
  }

  def factionOf(playerId: Int) = fromId(playerId).get.faction

  def handOf(playerId: Int) = fromId(playerId).get.hand

  def discardOf(playerId: Int) = fromId(playerId).get.discard

  def pickChancellor(playerId: Int): PlayerGroup = {
    val nextPlayers = players.map(player =>
      if (player.id != playerId) player
      else player.copy(isChancellor = true)
    )

    PlayerGroup(nextPlayers)
  }

  val haveVoted = players.forall(_.vote.isDefined)

  val votePassed = players.count(_.vote.getOrElse(false) == true) >= 3

  val votes: Map[Boolean, Set[Int]] = players.groupBy(_.vote getOrElse false).mapValues(_.map(_.id))

  def hand(hand: Hand, id: Int) = {
    val nextPlayers = players.map(player =>
      if (player.id == id) player.copy(hand = Some(hand))
      else player
    )

    PlayerGroup(nextPlayers)
  }

  def discard(faction: Faction, id: Int) = {
    val nextPlayers = players.map(player =>
      if (player.id == id) player.copy(discard = Some(faction))
      else player
    )

    PlayerGroup(nextPlayers)
  }
}

object PlayerGroup {
  val numberOfLiberals = 3
  val numberOfFascists = 2

  def reset: PlayerGroup = {
    val liberals = List.fill(numberOfLiberals)(Player(0, Liberal, false, false, None, None, true, None))
    val fascists = List.fill(numberOfFascists)(Player(0, Fascist, false, false, None, None, true, None))

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
                  hand: Option[Hand],
                  discard: Option[Faction],
                  eligibleForChancellor: Boolean,
                  vote: Option[Boolean])

case class Turn(presidentId: Int,
                chancellorId: Int,
                votes: Map[Boolean, Set[Int]],
                pHand: Option[Hand],
                pDiscard: Option[Faction],
                cHand: Option[Hand],
                cDiscard: Option[Faction],
                enactedPolicy: Option[Faction])

case class Board(fascists: Int, liberals: Int, lastEnactedPolicy: Option[Faction]) {
  def enactPolicy(faction: Faction): Board = faction match {
    case Fascist => Board(fascists + 1, liberals, Some(Fascist))
    case Liberal => Board(fascists, liberals + 1, Some(Liberal))
  }

  val wasPolicyJustEnacted = lastEnactedPolicy.isDefined
}

object Board {
  def reset = Board(0, 0, None)
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

  def canDiscard(faction: Faction) = faction match {
    case Fascist => fascists > 0
    case Liberal => liberals > 0
  }

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
