package com.agario.commands

import com.agario.models.{BaseEntity, Fragment, World}
import com.agario.navigation.Track

class EatFood(fragment: Fragment, entity : BaseEntity, track : Track, startTick : Int)
  extends Command(fragment, entity, track, startTick) {

  override def isFinished() : Boolean = {
    (
      (super.isFinished())
        ||
      (World.getEntity(entity.getId()).isEmpty && Fragment.isVisible(World.fragments.values, entity))
    )
  }

  /*
  if (World.getPlayers().size == 0 &&
          World.fragments.size < World.config.maxFragmentsCount &&
          World.fragments.values.filter(f => f.weight >= Config.minWeightToBurst).size > 0) {
      return new Response(point.x, point.y, true, false)
    }
   */
}