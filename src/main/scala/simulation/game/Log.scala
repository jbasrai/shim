package simulation.game

import simulation.entities.{Board, Faction}

/**
  * Created by jbasrai on 4/13/17.
  */
case class Log(board: Board,
               presidentId: Int,
               chancellorId: Int,
               votes: Map[Int, Boolean],
               policyEnacted: Option[Faction])
