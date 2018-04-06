package com.agario.actions

import com.agario.Config
import com.agario.commands.{Command, Empty, Move}
import com.agario.models.{Fragment, Virus, World}
import com.agario.utils.Point

class ActionVirusBurst(virusId : String, fragmentId : String, world: World) extends Action {

  def run(): Command = {
    val fragment = world.fragments.get(fragmentId)
    val virus = world.viruses.get(virusId)

    if (fragment.isEmpty || virus.isEmpty) {
      return new Empty(Point.zero())
    }

    new Move(virus.get.circle.point)
  }

  def isEnd(): Boolean = {
    val fragment = world.fragments.get(fragmentId)
    val virus = world.viruses.get(virusId)

    if (fragment.isEmpty || virus.isEmpty) {
      return true
    }

    false
  }
}


object ActionVirusBurst {

  def searchVirus (fragments : Map[String , Fragment], world: World): Option[(Fragment, Virus)] = {
    if (world.viruses.size == 0 && fragments.size >= world.config.maxFragmentsCount) {
      return None
    }

    val minTicksVirus = world.fragments.values.
      filter(f => f.circle.r >= world.config.virusRadius && f.weight >= Config.minWeightToBurst).
      map{
      case f =>
        (
          f,
          world.viruses.values.map{
              case v => (Fragment.positionTick(f, v.circle.point, world.config)._1 , v)
          }.minBy(_._1)
        )
    }
    if (minTicksVirus.size == 0)
      None
    else
      Some((minTicksVirus.minBy(_._2._1)._1, minTicksVirus.minBy(_._2._1)._2._2))
  }
}