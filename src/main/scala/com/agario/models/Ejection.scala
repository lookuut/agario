package com.agario.models

import com.agario.navigation.Track
import com.agario.utils.{Circle, Point, Trajectory}


class Ejection (
               id: String,
               var posCircle : Circle,
               var speed : Point,
               var weight : Double,
               var pId : Int
               ) extends BaseEntity(id, eType = BaseEntity.ejection) {

  var inertion : Double = BaseEntity.inertion(weight)
  var maxSpeed : Double = BaseEntity.maxSpeed(weight)

  override def copy(): BaseEntity = {
    new Ejection(id, posCircle, speed, weight, pId)
  }

  override def isStatic(): Boolean = false

  override def factor(fragment: Fragment): Map[Point, Double] = {

    val factor = factorValue(fragment)

    Map(fragment.field.pointCell(posCircle.point) -> (factor))
  }

  override def factorValue(fragment : Fragment) : Double = {
    if (pId == fragment.playerId)
      Ejection.ownEjectionFactor
    else
      Ejection.enemyEjectionFactor
  }

  override def fadingFactor(lastFactors : Map[Point, Double]): Map[Point, Double] = {
    Map.empty[Point, Double]
  }
  override def canEat(fragment: Fragment): Boolean = true
}


object Ejection {
  val covertPart = 2f/3
  val ownEjectionFactor = -1f
  val enemyEjectionFactor = -2f
  val radius = 4f
  val weight = 15f
}