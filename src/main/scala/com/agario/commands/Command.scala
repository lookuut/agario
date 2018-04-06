package com.agario.commands

import com.agario.models.Track
import com.agario.utils.Point

abstract class Command(val point : Point, val track : Track) {
  def this(point : Point) = this(point, Track.empty())
}
