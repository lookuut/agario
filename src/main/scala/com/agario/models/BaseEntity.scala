package com.agario.models

import com.agario.utils.{Circle, Point}

abstract class BaseEntity(val id : String, val eType : String) {

  var posCircle : Circle
  var speed : Point
  var weight : Double
  var inertion : Double
  var maxSpeed : Double

  def isStatic() : Boolean

  def getId(): String = {
    eType.toString + id
  }

  def copy() : BaseEntity

  def update(posCircle : Circle, speed : Point, weight : Double): BaseEntity = {
    this.posCircle = posCircle
    this.speed = speed
    this.weight = weight
    this.inertion = BaseEntity.inertion(weight)
    this.maxSpeed = BaseEntity.maxSpeed(weight)

    this
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: BaseEntity => that.getId() == getId()
      case _ => false
    }

  override def hashCode() : Int = {
    getId().hashCode
  }

  def canEat (fragment: Fragment): Boolean
  def fadingFactor(lastFactor : Map[Point, Double]) : Map[Point, Double]
  def factor(fragment: Fragment) :  Map[Point, Double]
  def factorValue(fragment : Fragment) : Double
}

object BaseEntity {
  val fragment = "FA"
  val player = "P"
  val food = "F"
  val ejection = "E"
  val virus = "V"

  def inertion(weight : Double) : Double = World.config.inertionFactor / weight
  def maxSpeed(weight : Double): Double = World.config.speedFactor / math.sqrt(weight)

  def getEntityId(e : Entity): String = {
    if (e.objectType.isEmpty) {
      fragment + e.id.get
    } else if (e.objectType.get == player) {
      player + e.id.get
    } else if (e.objectType.get == food) {
      food + e.point.toString
    } else if (e.objectType.get == ejection) {
      ejection + e.id.get
    } else if (e.objectType.get == virus) {
      virus + e.id.get
    } else {
      throw new Exception("Unknown type " + e.objectType.get)
    }
  }
}
