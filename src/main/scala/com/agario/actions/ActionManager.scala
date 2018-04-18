package com.agario.actions

import com.agario.Config
import com.agario.commands._
import com.agario.models.{Fragment, Response, World}
import com.agario.utils.{Point, Profiler}

/**
  * @note action then navigation manager
  */
class ActionManager() {

  var action = new ActionMove()

  def run () : Response = {

    val command = action.run()

    val point = navigation(command)

    if (World.getPlayers().size == 0 &&
          World.fragments.size < World.config.maxFragmentsCount &&
          World.fragments.values.filter(f => f.weight >= Config.minWeightToBurst).size > 0) {
      return new Response(point.x, point.y, true, false)
    }

    new Response(point.x, point.y, command.isInstanceOf[Split], command.isInstanceOf[Eject])
  }


  def navigation (command : Command): Point = {

    val fragment = World.getMinDistanceFragment(command.entity.posCircle.point)

    if (fragment.isEmpty) {
      throw new Exception("No fragments to navigate")
    }

    val step = command.track.getStep(World.tick - command.startTick + 1)
    if (step.isDefined) {
      (step.get.direction * 100 + fragment.get.posCircle.point)
    } else {
      (fragment.get.posCircle.point)
    }
  }
}
