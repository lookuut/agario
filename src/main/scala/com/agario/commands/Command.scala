package com.agario.commands

import com.agario.models.{BaseEntity, Fragment, World}
import com.agario.navigation.Track
import com.agario.utils.Point


abstract class Command(val fragment: Fragment, val entity : BaseEntity, val track : Track, val startTick : Int) {
  def run() : (Point, Point, Boolean) = {//from pos, direction, split

    if (isFinished()) {
      (fragment.posCircle.point, entity.posCircle.point - fragment.posCircle.point, false)
    } else {
      if (track.getStep(World.tick - startTick).isEmpty) {
        (fragment.posCircle.point, entity.posCircle.point - fragment.posCircle.point, false)
      } else {
        val dir = track.getStep(World.tick - startTick).get.direction
        (fragment.posCircle.point, dir, false)
      }
    }
  }

  def isFinished() : Boolean = {
    startTick + track.duration() < World.tick
  }
}
