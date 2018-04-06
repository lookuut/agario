package com.agario.commands

import com.agario.models.Track
import com.agario.utils.Point

class Move(point : Point, track : Track) extends Command(point, track) {
  def this(point : Point) = this(point, Track.empty())
}
