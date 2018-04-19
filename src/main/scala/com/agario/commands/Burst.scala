package com.agario.commands

import com.agario.models.{BaseEntity, Fragment, World}
import com.agario.navigation.Track


class Burst(fragment: Fragment, entity : BaseEntity, track : Track, startTick : Int)
  extends Command(fragment, entity, track, startTick) {

}
