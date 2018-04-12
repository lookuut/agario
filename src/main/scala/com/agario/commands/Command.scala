package com.agario.commands

import com.agario.navigation.Track
import com.agario.utils.Point

abstract class Command(val point : Point, val track : Option[Track]) {
  def this(point : Point) = this(point, Some(Track.empty()))
}
