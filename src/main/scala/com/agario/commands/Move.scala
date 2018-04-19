package com.agario.commands

import com.agario.models.{BaseEntity, Fragment, World}
import com.agario.navigation.Track
import com.agario.utils.Circle



class Move(fragment: Fragment, entity : BaseEntity, track : Track, startTick : Int)
  extends Command(fragment, entity, track, startTick) {

  override def isFinished() : Boolean = {

    (startTick + track.duration() < World.tick ||
      (World.tick % 5 == 0 && World.fragments.values.
        filter(f => Circle.isCoverPoint(f.posCircle.point, f.posCircle.r + f.posCircle.r,track.getEndPoint())).size > 0
        )
      )
  }
}
