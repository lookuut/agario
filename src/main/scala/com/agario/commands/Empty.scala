package com.agario.commands

import com.agario.models.{BaseEntity, Fragment, World}
import com.agario.navigation.Track



class Empty(fragment: Fragment, entity : BaseEntity, track : Track, startTick : Int)
  extends Command(fragment, entity, track, startTick) {

  override def isFinished() : Boolean = {
    startTick + track.duration() < World.tick
  }
}
