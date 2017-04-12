package simulation.entities

import simulation._

import scala.util.Random

/**
  * Created by jbasrai on 4/11/17.
  */
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