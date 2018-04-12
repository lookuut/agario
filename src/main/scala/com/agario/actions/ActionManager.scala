package com.agario.actions

import com.agario.Config
import com.agario.commands._
import com.agario.models.{Fragment, Response, World}
import com.agario.utils.{Point, Profiler}

/**
  * @note action then navigation manager
  */
class ActionManager(world: World) {

  var action : Option[Action] = None

  def run (world : World) : Response = {
    if (action.isEmpty || action.get.isEnd() || world.isWorldChanged) {
      action = Some(newAction())
    }

    val command = action.get.run()

    val point = navigation(command)

    if (world.getPlayers().size == 0 &&
          world.fragments.size < world.config.maxFragmentsCount &&
          world.fragments.values.filter(f => f.weight >= Config.minWeightToBurst).size > 0) {
      return new Response(point.x, point.y, true, false)
    }

    new Response(point.x, point.y, command.isInstanceOf[Split], command.isInstanceOf[Reject])
  }


  def navigation (command : Command): Point = {

    val fragment = world.getMinDistanceFragment(command.point)

    if (fragment.isEmpty) {
      throw new Exception("No fragments to navigate")
    }

    if (command.isInstanceOf[Move] && command.track.isDefined) {
      val step = command.track.get.getStep(world.tick + 1)
      (step.direction * 100 + fragment.get.posCircle.point)
    } else {
      if (Fragment.positionTick(fragment.get, command.point, world.config)._1 >
        Fragment.moveWithCorrectionTick(fragment.get, command.point, world.config)._1) {

        val targetVec = (command.point - fragment.get.posCircle.point)
        val direction = Fragment.getCorrectionDirect(fragment.get.speed, targetVec.normalize(),fragment.get.maxSpeed(world.config)).normalize()
        direction * targetVec.length() + fragment.get.posCircle.point
      } else {
        command.point
      }
    }
  }

  def newAction(): Action = {

    val virus = ActionVirusBurst.searchVirus(world.fragments, world)

    if (virus.isDefined && world.getPlayers().size == 0 && world.fragments.head._2.weight > Config.minWeightToBurst) {
      return new ActionVirusBurst(virus.get._2.id, virus.get._1.id, world)
    }

    return new ActionMove(world)
  }
}
