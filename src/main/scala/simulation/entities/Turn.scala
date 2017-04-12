package simulation.entities

/**
  * Created by jbasrai on 4/11/17.
  */
case class Turn(presidentId: Int,
                chancellorId: Int,
                votes: Map[Boolean, Set[Int]],
                pHand: Option[Hand],
                pDiscard: Option[Faction],
                cHand: Option[Hand],
                cDiscard: Option[Faction],
                enactedPolicy: Option[Faction])
