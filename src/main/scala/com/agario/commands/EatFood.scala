package com.agario.commands

import com.agario.models.{BaseEntity, Fragment, World}
import com.agario.navigation.Track

class EatFood(entity : BaseEntity, track : Track, startTick : Int) extends Command(entity, track, startTick) {
  def isFinished() : Boolean = {

    (
      (startTick + track.duration() < World.tick)
        ||
      (World.getEntity(entity.getId()).isEmpty && Fragment.isVisible(World.fragments.values, entity))
    )
  }
}