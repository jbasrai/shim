package simulation

import strategy._
import entities._

/**
  * Created by jbasrai on 4/10/17.
  */
class Simulation(strategies: Map[Faction, Strategy], numberOfGames: Int) {
  // main entry point of the project
  // runs the simulation from start to finish, over <numberOfGames> trials
  // returns a reverse-ordered list of final game states, each also containing the complete game log
  def start: List[Game] = runSimulation(1, Nil)

  private def runSimulation(gameCount: Int, results: List[Game]): List[Game] = {
    if (gameCount > numberOfGames)
      results
    else
      runSimulation(gameCount + 1, simulateGame(Game.reset(strategies)) :: results)
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
    else if (game.players.havePassedVote && !game.board.wasPolicyJustEnacted)
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
    val pDiscardStrat = game.players.presidentDiscardStrategy
    val pHand = game.deck.draw
    assert(pHand.size == 3)

    val pDiscard = pDiscardStrat(game, pHand)

    // then the chancellor discards a card and enacts the remaining policy
    val cDiscardStrat = game.players.chancellorDiscardStrategy
    val cHand = pHand - pDiscard
    assert(cHand.size == 2)

    val cDiscard = cDiscardStrat(game, cHand)

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

object Simulation {
  def main(args: Array[String]): Unit = {
    val trials = 1000
    val sim = new Simulation(Map(
      Liberal -> DefaultLiberalStrategy,
      Fascist -> SimpleFascistStrategy
    ), trials)
    val results = sim.start

    println(results.head.log)

    val liberalWins = results.map(_.winner).count(_ == Liberal)

    val fascistFrequencyMap = results.map(_.board.fascists).groupBy(x => x).mapValues(_.length).toList.sortBy(_._1)

    println(s"Liberal winrate: $liberalWins / $trials")
    for ((fascistPolicesEnacted, frequency) <- fascistFrequencyMap) yield {
      println(s"$fascistPolicesEnacted fascist policies enacted $frequency / $trials of the time")
    }
  }
}
