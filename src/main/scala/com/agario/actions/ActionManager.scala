package com.agario.actions

import com.agario.models.{Response, World}
import com.agario.utils.{Line}

/**
  * @note action then navigation manager
  */
class ActionManager() {

  var action = new ActionMove()

  def run () : Response = {

    val command = action.run()

    var (pos, dir, isSplit) = command.run()

    val point =
      if (!(dir.x == 0 && dir.y == 0))
        Line.pointCrossWithBorder(pos, dir, World.config.width, World.config.height)
      else
        pos

    if (World.tick % 40 == 0) {
      val minWeight = World.fragments.values.map(f => f.weight).min
      val dangerPlayers = World.players.values.filter(p => p.weight >= minWeight / (2 * 1.2) )
      if (dangerPlayers.size == 0 && World.fragments.values.filter(_.canSplit()).size > 0) {
         isSplit = true
      }
    }

    new Response(point.x, point.y, isSplit, false)
  }

}
