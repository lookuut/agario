package com.agario.actions

import com.agario.Config
import com.agario.commands.{Command, Empty, Move}
import com.agario.models.{BaseEntity, Fragment, Virus, World}
import com.agario.navigation.Track
import com.agario.utils.Point
/*
class ActionVirusBurst(virusId : String, fragmentId : String) extends Action {

  def run(): Command = {
    val fragment = World.fragments.get(fragmentId)
    val virus = World.getEntities(BaseEntity.virus).get(virusId)

    if (fragment.isEmpty || virus.isEmpty) {
      return new Empty(Point.zero, Track.empty, World.tick)
    }

    new Move(virus.get.posCircle.point, Track.empty, World.tick)
  }

  def isEnd(): Boolean = {
    val fragment = World.fragments.get(fragmentId)
    val virus = World.getEntities(BaseEntity.virus).get(virusId)

    if (fragment.isEmpty || virus.isEmpty) {
      return true
    }

    false
  }
}


object ActionVirusBurst {

  def searchVirus (fragments : Map[String , Fragment]): Option[(Fragment, BaseEntity)] = {
    if (World.getEntities(BaseEntity.virus).size == 0 && fragments.size >= World.config.maxFragmentsCount) {
      return None
    }

    val minTicksVirus = World.fragments.values.
      filter(f => f.posCircle.r >= World.config.virusRadius && f.weight >= Config.minWeightToBurst).
      map{
      case f =>
        (
          f,
          World.getEntities(BaseEntity.virus).values.map{
              case v => (Fragment.positionTick(f, v.posCircle.point, World.config)._1 , v)
          }.minBy(_._1)
        )
    }
    if (minTicksVirus.size == 0)
      None
    else
      Some((minTicksVirus.minBy(_._2._1)._1, minTicksVirus.minBy(_._2._1)._2._2))
  }
}*/