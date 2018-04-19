package com.agario.commands

import com.agario.models.{BaseEntity, Fragment, World}
import com.agario.navigation.Track
import com.agario.utils.Point


class Escape(fragment: Fragment, entity : BaseEntity, track : Track, startTick : Int)
  extends Command(fragment, entity, track, startTick) {

  var safetyDir : Point = null
  var escapeTick = 0
  override def run() : (Point, Point, Boolean)  = {

    if (safetyDir == null || World.tick - escapeTick > 50) {
      escapeTick = World.tick

      val speedVec = (if(entity.speed.length() == 0) (entity.posCircle.point - fragment.posCircle.point) else entity.speed).normalize()
      val dir1 = speedVec.turn(math.Pi/2)
      val dir2 = speedVec.turn(-math.Pi/2)

      val l1 = speedVec.turn(math.Pi/2) + fragment.posCircle.point - World.mapCenter
      val l2 = speedVec.turn(math.Pi/2) + fragment.posCircle.point - World.mapCenter
      val dir = if (l1.length() < l2.length()) dir1 else dir2
      safetyDir = dir * 50
    }


    return (fragment.posCircle.point, (safetyDir), false)
  }

  def edgeFactor (cell : Point): Double = {
    if (cell.x == 0 || cell.y == 0 || cell.x == World.staticEntitiesField.width - 1 || cell.y == World.staticEntitiesField.height - 1) 100.0 else 0.0
  }

  override def isFinished() : Boolean = {
    if (World.fragments.get(fragment.getId()).isEmpty) {
      return true
    }
    val cell = fragment.fragmentCell
    val f = (
      fragment.fadingField.getFactors(Set(cell)).get(cell).get + fragment.field.getFactors(Set(cell)).get(cell).get +
        (if (cell.x == 0 || cell.y == 0 || cell.x == World.staticEntitiesField.width - 1 || cell.y == World.staticEntitiesField.height - 1) 100.0 else 0.0)
      )


    f <= 0
  }
}
