package simulation.entities

/**
  * Created by jbasrai on 4/11/17.
  */
// this is not a List[Policy] in order to relax equality and allow pattern matching
// for now, assume order of the cards in hand does not matter
case class Hand(fascists: Int, liberals: Int) {
  assert(fascists >= 0 && liberals >= 0)

  // does not protect against invalid hands, discard responsibly
  def discard(faction: Faction): Hand = faction match {
    case Fascist => Hand(fascists - 1, liberals)
    case Liberal => Hand(fascists, liberals - 1)
  }

  def -(faction: Faction): Hand = this discard faction

  def canDiscard(faction: Faction) = faction match {
    case Fascist => fascists > 0
    case Liberal => liberals > 0
  }

  // should only be called when size is 1
  val card = if (fascists == 1) Fascist else Liberal

  val size = fascists + liberals
}

object Hand {
  // helps simply construction when hand is drawn from deck: List[Policy]
  def fromList(policies: List[Policy]): Hand =
  Hand(policies count (_ == Policy(Fascist)),
    policies count (_ == Policy(Liberal)))
}
