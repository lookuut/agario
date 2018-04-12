package com.agario.models

import com.agario.navigation.Track
import com.agario.utils.{Circle, Point, Trajectory}


class Ejection (
               world: World,
               id: String,
               var posCircle : Circle,
               var speed : Point,
               var weight : Double,
               var pId : Int
               ) extends BaseEntity(id, eType = BaseEntity.ejection, world) {
  override def isStatic(): Boolean = false

  override def factor(fragment: Fragment): (Map[Point, Double], Track) = {

    val factor = if (pId == fragment.playerId) Ejection.ownEjectionFactor else Ejection.enemyEjectionFactor
    val track = Trajectory.searchTrack(world, fragment, posCircle, Food.coverPart)

    (Map(fragment.field.pointCell(posCircle.point) -> (factor) / (1 + track.duration())), track)
  }

  override def fadingFactor(lastFactors : Map[Point, Double]): Map[Point, Double] = {

    if (world.
      fragments.
      values.
      filter(
        f =>
          f.posCircle.
            isCover(posCircle, Ejection.covertPart)
      ).size == 0
    ) { //some one eat ejection, add ejection pos as visible
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


object Ejection {
  val covertPart = 2f/3
  val ownEjectionFactor = -1f
  val enemyEjectionFactor = -2f
  val radius = 4f
  val weight = 15f
}