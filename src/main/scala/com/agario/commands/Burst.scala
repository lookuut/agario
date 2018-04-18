package com.agario.commands

import com.agario.models.{BaseEntity, World}
import com.agario.navigation.Track

class Burst(entity : BaseEntity, track : Track, startTick : Int) extends Command(entity, track, startTick) {
  def isFinished() : Boolean = {
    startTick + track.duration() > World.tick
  }
}
