package com.agario.commands

import com.agario.models.BaseEntity
import com.agario.navigation.Track


abstract class Command(val entity : BaseEntity, val track : Track, val startTick : Int) {
  def isFinished() : Boolean
}
