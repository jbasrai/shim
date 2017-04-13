package simulation.strategy

import simulation.entities._
import simulation.game.{Game, PolicyEnaction}

/**
  * Created by jbasrai on 4/11/17.
  */
trait Strategy {
  def presidentDiscardStrategy(game: PolicyEnaction, hand: Hand): Faction

  def chancellorDiscardStrategy(game: PolicyEnaction, hand: Hand): Faction
}

