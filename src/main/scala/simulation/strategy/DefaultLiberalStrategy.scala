package simulation.strategy
import simulation.entities._
import simulation.game.{Game, PolicyEnaction}

/**
  * Created by jbasrai on 4/11/17.
  */
object DefaultLiberalStrategy extends BaseStrategy {
  override def presidentDiscardStrategy(game: PolicyEnaction, hand: Hand): Faction =
    discardFascistIfPossible(hand)

  override def chancellorDiscardStrategy(game: PolicyEnaction, hand: Hand): Faction =
    discardFascistIfPossible(hand)
}
