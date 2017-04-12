package simulation.strategy

import simulation.entities._

/**
  * Created by jbasrai on 4/11/17.
  */
object SimpleFascistStrategy extends FascistStrategy {
  override def presidentDiscardStrategy(game: Game, hand: Hand): Faction =
    discardLiberalIfPossible(hand)

  override def chancellorDiscardStrategy(game: Game, hand: Hand): Faction =
    discardFascistIfPossible(hand)
}
