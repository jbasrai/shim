package simulation.entities

/**
  * Created by jbasrai on 4/11/17.
  */
case class Board(fascists: Int, liberals: Int) {
  def enactPolicy(faction: Faction) = faction match {
    case Fascist => Board(fascists + 1, liberals)
    case Liberal => Board(fascists, liberals + 1)
  }
}

object Board {
  def reset = Board(0, 0)
}