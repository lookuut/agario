package com.agario.models

import com.agario.utils.{Circle, Point}

class Player (val id : String,
              val circle : Circle,
              val weight : Double,
              val speed : Point
            )
{

  override def equals(that: Any): Boolean =
    that match {
      case that: Player => that.id == id
      case _ => false
    }

  override def hashCode() : Int = {
    id.hashCode
  }
}


object Player {
  def speedByLastPos(currentPos : Point, lastPos : Point) : Point = {
     (currentPos - lastPos)
  }
}