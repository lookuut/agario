package com.agario.actions

import com.agario.commands.{Command, Empty, Move}
import com.agario.models.{Fragment, World}
import com.agario.navigation.Track
import com.agario.utils.Point

/*
class ActionUnite(fragmentIds: Iterable[String]) extends Action {

  def run () : Command = {

    val fragments = fragmentIds.filter(id => World.fragments.contains(id)).map(id => World.fragments.get(id))

    if (fragments.size > 0) {
      val fragmentsCenter = fragments.map(_.get.posCircle.point).reduce(_ + _) * (1.0 / fragments.size)
      new Move(fragmentsCenter, Track.empty, World.tick)
    } else {
      new Empty(Point.zero, Track.empty, World.tick)
    }
  }

  def isEnd(): Boolean = {
    fragmentIds.filter(id => World.fragments.contains(id)).size == 0
  }
}

object ActionUnite {
  def ttfFragments(fragments : Map[String, Fragment]) : Map[Int, (Point, Double)] = {
    fragments.
      values.
      map(f => (f.ttf, f)).
      groupBy(_._1).
      map{
        case (ttf, ttfFragments) =>
          val ttfAveragePoint = ttfFragments.map(_._2.posCircle.point).reduce(_ + _) * (1.0 / ttfFragments.size)
          val ttfWeight = ttfFragments.map(_._2.weight).sum

          (ttf, (ttfAveragePoint, ttfWeight))
      }
  }
}
*/