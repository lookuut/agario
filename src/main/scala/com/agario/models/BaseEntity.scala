package com.agario.models

import com.agario.navigation.Track
import com.agario.utils.{Circle, Point}

abstract class BaseEntity(val id : String, val eType : String, val world : World) {

  var posCircle : Circle
  var speed : Point
  var weight : Double

  def isStatic() : Boolean

  def getId(): String = {
    eType.toString + id
  }

  def update(posCircle : Circle, speed : Point, weight : Double): BaseEntity = {
    this.posCircle = posCircle
    this.speed = speed
    this.weight = weight
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

  def fadingFactor(lastFactor : Map[Point, Double]) : Map[Point, Double]
  def factor(fragment: Fragment) : (Map[Point, Double], Track)
}

object BaseEntity {
  val fragment = "FA"
  val player = "P"
  val food = "F"
  val ejection = "E"
  val virus = "V"

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
