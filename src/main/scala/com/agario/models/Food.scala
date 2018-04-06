package com.agario.models

import com.agario.utils.{Circle}

class Food (val circle : Circle,
            val weight : Double
               ) {

  override def equals(that: Any): Boolean =
    that match {
      case that: Food => that.circle == circle
      case _ => false
    }

  override def hashCode() : Int = {
    var bits: Long = 7L
    bits = 31L * bits + java.lang.Double.doubleToRawLongBits(circle.point.x)
    bits = 31L * bits + java.lang.Double.doubleToRawLongBits(circle.point.y)
    (bits ^ (bits >> 32)).toInt
  }
}


object Food {
  val coverPart = 2.0f / 3
  val radius = 2.5f
}
