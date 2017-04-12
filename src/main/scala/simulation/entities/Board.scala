package simulation.entities

/**
  * Created by jbasrai on 4/11/17.
  */
case class Board(fascists: Int, liberals: Int, lastEnactedPolicy: Option[Faction]) {
  def enactPolicy(faction: Faction): Board = faction match {
    case Fascist => Board(fascists + 1, liberals, Some(Fascist))
    case Liberal => Board(fascists, liberals + 1, Some(Liberal))
  }

  val wasPolicyJustEnacted = lastEnactedPolicy.isDefined
}

object Board {
  def reset = Board(0, 0, None)
}