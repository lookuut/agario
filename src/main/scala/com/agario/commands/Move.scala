package com.agario.commands

import com.agario.navigation.Track
import com.agario.utils.Point

class Move(point : Point, track : Option[Track]) extends Command(point, track) {
  def this(point : Point) = this(point, Some(Track.empty()))
}
