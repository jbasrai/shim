package simulation.strategy

import simulation.entities._

/**
  * Created by jbasrai on 4/11/17.
  */
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
