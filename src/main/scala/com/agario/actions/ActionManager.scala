package com.agario.actions

import com.agario.Config
import com.agario.commands._
import com.agario.models.{Fragment, Player, Response, World}
import com.agario.utils.{Circle, Point}

/**
  * @note action then navigation manager
  */
class ActionManager(world: World) {

  var action : Action = new ActionEatFood(world)

  def run (world : World) : Response = {

    if (world.isWorldChanged && !action.isInstanceOf[ActionEscape]) {
      action = newAction()
    } else if (action.isEnd()) {
      action = newAction()
    }

    val command = action.run()

    val point = navigation(command)

    if (world.players.size == 0 &&
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

    if (world.fragments.size > 1 && action.isInstanceOf[ActionEatFood]) {
      val fragment = world.getMinDistanceFragment(command.point)
      return (command.point - fragment.get.circle.point) * 100
    }

    if (command.isInstanceOf[Move] && command.track.endTick > world.tick) {
      val step = command.track.getStep(world.tick)
      (step.direction * 100 + fragment.get.circle.point)
    } else {
      if (Fragment.positionTick(fragment.get, command.point, world.config)._1 >
        Fragment.moveWithCorrectionTick(fragment.get, command.point, world.config)._1) {

        val targetVec = (command.point - fragment.get.circle.point)
        val direction = Fragment.getCorrectionDirect(fragment.get.speed, targetVec.normalize(),fragment.get.maxSpeed(world.config)).normalize()
        direction * targetVec.length() + fragment.get.circle.point
      } else {
        command.point
      }
    }


  }

  def newAction(): Action = {

    val escape = ActionEscape.isDangerAround(world)
    if (escape.isDefined) {
      return escape.get
    }

    val victim = ActionCatch.searchVictims(world)
    if (victim.isDefined) {
      return new ActionCatch(victim.get.id, world)
    }

    val virus = ActionVirusBurst.searchVirus(world.fragments, world)
    val actionEatFood = new ActionEatFood(world)
    val eatFoodCommand = actionEatFood.run()

    if (virus.isDefined && world.players.size == 0 && world.fragments.head._2.weight > Config.minWeightToBurst) {
      return new ActionVirusBurst(virus.get._2.id, virus.get._1.id, world)
    }

    if (eatFoodCommand.isInstanceOf[Move]) {
      return actionEatFood
    }

    return new ActionRandomMove(world)
  }
}
