package simulation.entities

/**
  * Created by jbasrai on 4/11/17.
  */
sealed trait Faction {
  val otherFaction: Faction
}

case object Fascist extends Faction {
  override val otherFaction: Faction = Liberal
}

case object Liberal extends Faction {
  override val otherFaction: Faction = Fascist
}
