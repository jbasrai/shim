package simulation.strategy

import simulation.entities._

/**
  * Created by jbasrai on 4/11/17.
  */
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
