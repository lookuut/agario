package com.agario.actions

import com.agario.Strategy
import com.agario.commands.{Command, Move}
import com.agario.models.World
import com.agario.utils.Point

class ActionRandomMove(world : World) extends Action {
  val point = new Point(
                    Strategy.rand.nextInt(world.config.width - 2 * world.getMaxRadius().toInt) + world.getMaxRadius().toInt ,
                    Strategy.rand.nextInt(world.config.height - 2 * world.getMaxRadius().toInt) + world.getMaxRadius().toInt
              )

  def run () : Command = {
    new Move(point)
  }

  def isEnd(): Boolean = {
    world.fragments.values.filter(f => f.circle.point.distance(point) <= f.circle.r).size > 0
  }
}
