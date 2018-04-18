package com.agario.models

import com.agario.utils.{Circle, Point}

class Virus (id : String,
             var posCircle : Circle,
             var speed : Point,
             var weight : Double
               ) extends BaseEntity (id,  eType = BaseEntity.virus){

  var inertion : Double = BaseEntity.inertion(weight)
  var maxSpeed : Double = BaseEntity.maxSpeed(weight)

  override def copy(): BaseEntity = {
    new Virus(id, posCircle, speed, weight)
  }

  override def isStatic(): Boolean = true

  override def canEat(fragment: Fragment): Boolean = {
    (
      fragment.weight >= Virus.burstMinWeight
      &&
      fragment.posCircle.r >= posCircle.r
      &&
      World.fragments.size < World.config.maxFragmentsCount
      )
  }

  override def factor(fragment: Fragment): Map[Point, Double] = {

    val point = fragment.field.pointCell(posCircle.point)
    val virusFactor = factorValue(fragment)

    if (virusFactor > 0) {
      Map(point -> -(virusFactor))
    } else {
      Map(point -> virusFactor)
    }
  }

  override def factorValue(fragment : Fragment) : Double = {
    if (
      fragment.weight >= Virus.burstMinWeight
        &&
        fragment.posCircle.r >= posCircle.r
        &&
        World.fragments.size < World.config.maxFragmentsCount
    )
      Virus.factor
    else 0f
  }

  override def fadingFactor(lastFactors : Map[Point, Double]): Map[Point, Double] = {
    Map.empty[Point, Double]
  }
}

object Virus {
  val weight = 40f
  val coverPart = 2f / 3
  val factor = 2.0
  val burstMinWeight = 120.0
}