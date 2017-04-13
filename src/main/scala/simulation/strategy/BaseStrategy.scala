package simulation.strategy

import simulation.entities._

/**
  * Created by jbasrai on 4/12/17.
  */
abstract class BaseStrategy extends Strategy {
  private def discardIfPossible(faction: Faction, hand: Hand) =
    if (hand canDiscard faction) faction
    else faction.otherFaction

  protected def discardFascistIfPossible(hand: Hand) =
    discardIfPossible(Fascist, hand)

  protected def discardLiberalIfPossible(hand: Hand) =
    discardIfPossible(Liberal, hand)
}
