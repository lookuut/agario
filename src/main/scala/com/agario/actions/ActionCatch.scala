package com.agario.actions

import com.agario.commands.{Command, Empty, Move, Split}
import com.agario.models.{Fragment, Player, World}
import com.agario.navigation.Track
import com.agario.utils.Point
/*
class ActionCatch (playerId : String) extends Action{

  def run () : Command = {

    val player = World.getPlayer(playerId)

    if (player.isEmpty) {
      return new Empty(Point.zero, Track.empty, World.tick)
    }

    val split = World.fragments.
      values.
      filter {
        case f =>
          val victimVec = (player.get.posCircle.point - f.posCircle.point).normalize()
          f.weight / player.get.weight > 2 * Player.predatorFactor &&
          f.speed.angle(victimVec) <= math.Pi / 12
      }.size > 0

    if (split) {
      new Split(player.get.posCircle.point, Track.empty, World.tick)
    } else {
      new Move(player.get.posCircle.point, Track.empty, World.tick)
    }
  }

  def isEnd () : Boolean = {

    val player = World.getPlayer(playerId)

    if (player.isEmpty) {
      return true
    }

    val predators = World.fragments.values.filter(f => f.weight / Player.predatorFactor > player.get.weight)

    predators.map{//@TODO use viruses
      case p =>
        (
          (if (p.posCircle.point.distance(World.mapCenter) > player.get.posCircle.point.distance(World.mapCenter)) 0 else 1),
          p.weight / player.get.weight
        )
    }.filter(t => t._1 == 1 || t._2 >  2 * Player.predatorFactor).size == 0
  }
}

object ActionCatch {

  def searchVictims () : Option[Player] = {
    val victims = World.
      getPlayers().
      map{case (p) =>
        val ticks = World.
          fragments.
          values.
          filter(f => f.weight / p.weight > Player.predatorFactor).
          map{
            case (f) =>
              val victimVec = (p.posCircle.point - f.posCircle.point).normalize()
              (Fragment.positionTick(f, p.posCircle.point, World.config)._1, f.speed.angle(victimVec))//@TODO doit better with angle speed, weight
          }
        (if (ticks.size > 0) ticks.minBy(_._1)._1 else 0, p)
      }.
      filter(_._1 > 0)

    return if (victims.size > 0) Some(victims.minBy(_._1)._2) else None
  }
}
*/