package com.agario.models

import com.agario.navigation.Track
import com.agario.utils.{Circle, Point, Trajectory}

class Food (
           world: World,
           id : String,
           var posCircle : Circle,
           var speed : Point,
           var weight : Double
           ) extends BaseEntity(id, eType = BaseEntity.food, world) {

  override def equals(that: Any): Boolean =
    that match {
      case that: Food => that.posCircle == posCircle
      case _ => false
    }

  override def hashCode() : Int = {
    var bits: Long = 7L
    bits = 31L * bits + java.lang.Double.doubleToRawLongBits(posCircle.point.x)
    bits = 31L * bits + java.lang.Double.doubleToRawLongBits(posCircle.point.y)
    (bits ^ (bits >> 32)).toInt
  }

  override def isStatic(): Boolean = true

  override def factor(fragment: Fragment): (Map[Point, Double], Track) = {

    val track = Trajectory.searchTrack(world, fragment, posCircle, Food.coverPart)

    val factor = if (track.duration() == 0) Food.fadingFactor else ((Food.factor) / (track.duration() + 1))

    (Map(fragment.field.pointCell(posCircle.point) -> factor), track)
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

    Map.empty[Point, Double]  }
}


object Food {
  val coverPart = 2.0 / 3
  val radius = 2.5
  val factor = -1.0
  val fadingFactor = 1.0
}
