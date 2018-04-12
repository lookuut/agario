package com.agario.models

import com.agario.navigation.Track
import com.agario.utils.{Circle, Point, Trajectory}

class Virus (world : World,
             id : String,
             var posCircle : Circle,
             var speed : Point,
             var weight : Double
               ) extends BaseEntity (id,  eType = BaseEntity.virus, world){

  override def isStatic(): Boolean = true

  override def factor(fragment: Fragment): (Map[Point, Double], Track) = {

    val point = fragment.field.pointCell(posCircle.point)
    val virusFactor = if (
                          fragment.weight >= Virus.burstMinWeight
                            &&
                            fragment.posCircle.r >= posCircle.r
                            &&
                          world.fragments.size < world.config.maxFragmentsCount
                      )
                      Virus.factor
                      else 0f

    if (virusFactor > 0) {
      val track = new Trajectory(world,fragment,this.posCircle, Virus.coverPart).straight()
      (Map(point -> -(virusFactor + 1) / (track.duration() + 1)), track)
    } else {
      (Map(point -> virusFactor), Track.empty())
    }
  }

  override def fadingFactor(lastFactors : Map[Point, Double]): Map[Point, Double] = {

    if (world.
      fragments.
      values.
      filter(f =>
        f.canBurstOnVirus()
          &&
        f.posCircle.isCover(this.posCircle, Virus.coverPart)
      ).size == 0)
    {//some one burst on virus, add his step ass visible
      world.addVisibleCells(
        world.
          field.
          getCircleCells(
            posCircle.point,
            world.lastVisiblePlayer.visionRadius
          ).toSet
      )
    }

    Map.empty[Point, Double]
  }
}

object Virus {
  val weight = 40f
  val coverPart = 2f / 3
  val factor = 2.0
  val burstMinWeight = 120.0
}