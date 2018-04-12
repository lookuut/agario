package com.agario.commands

import com.agario.navigation.Track
import com.agario.utils.Point

class Split(point : Point, track : Option[Track]) extends Command(point, track) {
  def this(point : Point) = this(point, Some(Track.empty()))
}
class Reject(point : Point, track : Option[Track]) extends Command(point, track) {
  def this(point : Point) = this(point, Some(Track.empty()))
}
