package com.agario.actions

import com.agario.commands.{Command, Empty, Move, Split}
import com.agario.models.{Fragment, Player, World}
import com.agario.utils.Point

class ActionCatch (playerId : String, world : World) extends Action{

  def run () : Command = {

    val player = world.getPlayer(playerId)

    if (player.isEmpty) {
      return new Empty(Point.zero())
    }

    val split = world.fragments.
      values.
      filter {
        case f =>
          val victimVec = (player.get.posCircle.point - f.posCircle.point).normalize()
          f.weight / player.get.weight > 2 * Player.predatorFactor &&
          f.speed.angle(victimVec) <= math.Pi / 12
      }.size > 0

    if (split) {
      new Split(player.get.posCircle.point)
    } else {
      new Move(player.get.posCircle.point)
    }
  }

  def isEnd () : Boolean = {

    val player = world.getPlayer(playerId)

    if (player.isEmpty) {
      return true
    }

    val predators = world.fragments.values.filter(f => f.weight / Player.predatorFactor > player.get.weight)

    predators.map{//@TODO use viruses
      case p =>
        (
          (if (p.posCircle.point.distance(world.mapCenter) > player.get.posCircle.point.distance(world.mapCenter)) 0 else 1),
          p.weight / player.get.weight
        )
    }.filter(t => t._1 == 1 || t._2 >  2 * Player.predatorFactor).size == 0
  }
}

object ActionCatch {

  def searchVictims (world : World) : Option[Player] = {
    val victims = world.
      getPlayers().
      map{case (p) =>
        val ticks = world.
          fragments.
          values.
          filter(f => f.weight / p.weight > Player.predatorFactor).
          map{
            case (f) =>
              val victimVec = (p.posCircle.point - f.posCircle.point).normalize()
              (Fragment.positionTick(f, p.posCircle.point,world.config)._1, f.speed.angle(victimVec))//@TODO doit better with angle speed, weight
          }
        (if (ticks.size > 0) ticks.minBy(_._1)._1 else 0, p)
      }.
      filter(_._1 > 0)

    return if (victims.size > 0) Some(victims.minBy(_._1)._2) else None
  }
}
