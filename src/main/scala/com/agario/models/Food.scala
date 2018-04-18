package com.agario.models

import com.agario.utils.{Circle, Point}

class Food (
           id : String,
           var posCircle : Circle,
           var speed : Point,
           var weight : Double
           ) extends BaseEntity(id, eType = BaseEntity.food) {

  var inertion : Double = BaseEntity.inertion(weight)
  var maxSpeed : Double = BaseEntity.maxSpeed(weight)

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

  override def copy(): BaseEntity = {
    new Food(id, posCircle, speed, weight)
  }

  override def canEat(fragment: Fragment): Boolean = true
  override def factor(fragment: Fragment): Map[Point, Double] = {

    val factor = Food.factor

    Map(fragment.field.pointCell(posCircle.point) -> factor)
  }

  override def factorValue(fragment : Fragment) : Double = {
    Food.factor
  }

  override def fadingFactor(lastFactors : Map[Point, Double]): Map[Point, Double] = {
    //lastFactors.map{case(p,f) => (p, math.abs(f))}
    Map.empty[Point, Double]
  }
}


object Food {
  val coverPart = 2.0 / 3
  val radius = 2.5
  val factor = -1.0
  val fadingFactor = 1.0
}
