package simulation.strategy

import simulation.entities._

/**
  * Created by jbasrai on 4/11/17.
  */
trait Strategy {
  def presidentDiscardStrategy(game: Game, hand: Hand): Faction

  def chancellorDiscardStrategy(game: Game, hand: Hand): Faction

  private def discardIfPossible(faction: Faction, hand: Hand) =
    if (hand canDiscard faction) faction
    else faction.otherFaction

  protected def discardFascistIfPossible(hand: Hand) =
    discardIfPossible(Fascist, hand)

  protected def discardLiberalIfPossible(hand: Hand) =
    discardIfPossible(Liberal, hand)
}

trait LiberalStrategy extends Strategy
trait FascistStrategy extends Strategy
