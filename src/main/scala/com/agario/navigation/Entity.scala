package com.agario.navigation

import com.agario.utils.{Circle, Point}

case class Entity(elType : Int, speed : Point, circle : Circle, weight : Double)

object Entity {
  val player = 1
  val food = 2
  val virus = 3
  val ejection = 4
  val fragment = 5
}