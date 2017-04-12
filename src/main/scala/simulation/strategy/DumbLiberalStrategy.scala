package simulation.strategy

import simulation.entities._

/**
  * Created by jbasrai on 4/11/17.
  */
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

