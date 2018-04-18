package com.agario.utils

import com.agario.models.{Player, World}


class Circle(val point : Point, val r : Double) {

  def square (): Double = {
    math.Pi * r * r
  }

  def isCoverPoint(p : Point): Boolean = {
    p.distance(point) <= r
  }

  def isCover(circle : Circle, coverPart : Double = 1.0f): Boolean = {
    return Circle.isCover(point, r, circle.point, circle.r, coverPart)
  }

  def canCover(circle: Circle): Boolean = {
    if (circle.r > r) {
      return false
    }

    if (circle.point.x < r && circle.point.y < r) {
      return circle.point.distance(new Point(r, r)) < r - circle.r
    } else if (circle.point.x < r && circle.point.y > World.config.height - r) {
      return circle.point.distance(new Point(r, World.config.height - r)) < r - circle.r
    } else if (circle.point.x > World.config.width - r && circle.point.y < r) {
      return circle.point.distance(new Point(World.config.width - r, r)) < r - circle.r
    } else if (circle.point.x > World.config.width - r && circle.point.y > World.config.height - r) {
      return circle.point.distance(new Point(World.config.width - r, World.config.height - r)) < r - circle.r
    }

    return true
  }

  override def toString() = f"""$point $r"""

  override def equals(that: Any): Boolean =
    that match {
      case that: Circle => that.point == point && that.r == r
      case _ => false
    }

  override def hashCode() : Int = {
    var bits: Long = 7L
    bits = 31L * bits + java.lang.Double.doubleToRawLongBits(point.x)
    bits = 31L * bits + java.lang.Double.doubleToRawLongBits(point.y)
    bits = 31L * bits + java.lang.Double.doubleToRawLongBits(r)

    (bits ^ (bits >> 32)).toInt
  }
}

object Circle {

  def isHeatBorder (point : Point, r : Double): Boolean = {
    if (
      point.x + r >= World.config.width || point.y + r >= World.config.height
        ||
      point.x <= r || point.y <= r
    ) {
      true
    } else {
      false
    }

  }

  def isCover(posPredator : Point, rPredator : Double, posVictim : Point, rVictim : Double, coverPart: Double) : Boolean = {
    if (rPredator < rVictim) {
      return false
    }

    val f = rVictim + ( rPredator - rPredator * 2 * coverPart)

    posVictim.squareDistance(posPredator) < f * f
  }

  def edges (r : Double) : Array[Circle] = {
    Array(
      new Circle(new Point(r, r), r),
      new Circle(new Point(World.config.width - r, r), r),
      new Circle(new Point(r, World.config.height - r), r),
      new Circle(new Point(World.config.width - r, World.config.width - r), r)
    )
  }

  val zero = new Circle(Point.zero, 0)

  def isIntersectTracks(victim : Circle, victimTrack : Map[Int, Point], predator : Circle, predatorTrack : Map[Int, Point], diameterPart : Double) : Boolean = {
    victimTrack.filter{
      case(tick , victimPoint) =>
        val predatorPoint = predatorTrack.getOrElse(tick, Point.zero)
        val f = predator.r + victim.r * 2 * ( 1.0f / 2 - diameterPart)
        val distance = predatorPoint.distance(victimPoint)
        distance < f
    }.size > 0
  }
}

