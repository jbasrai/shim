package simulation.entities

import simulation.strategy.Strategy

/**
  * Created by jbasrai on 4/11/17.
  */
case class Player(id: Int,
                  faction: Faction,
                  strategy: Strategy,
                  isPresident: Boolean,
                  isChancellor: Boolean,
                  hand: Option[Hand],
                  discard: Option[Faction],
                  eligibleForChancellor: Boolean,
                  vote: Option[Boolean])
