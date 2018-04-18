package com.agario.commands

import com.agario.models.{BaseEntity, World}
import com.agario.navigation.Track
import com.agario.utils.Point

class Split(entity : BaseEntity, track : Track, startTick : Int) extends Command(entity, track, startTick) {
  def isFinished() : Boolean = {
    startTick + track.duration() > World.tick
  }
}
class Eject(entity : BaseEntity, track : Track, startTick : Int) extends Command(entity, track, startTick) {
  def isFinished() : Boolean = {
    startTick + track.duration() > World.tick
  }
}
