package com.agario.actions

import com.agario.commands.{Command, Empty, Move, Split}
import com.agario.models.{Fragment, Player, World}
import com.agario.utils.{Circle, Point}


class ActionEscape(val playerId : String, world: World) extends Action {
  private val angleSectorCount = 24
  private val predatorEscapeDistance = 200
  private val mapCenter = new Point(world.config.width / 2, world.config.height / 2)
  var lastRunTick = -200
  var lastEscapeVariant : Command = new Empty(Point.zero())

  private val maxRunTick = 50
  def run () : Command = {//@TODO use split or reject for save fragment
  val player = world.players.get(playerId)

    //if (world.tick - lastRunTick > maxRunTick && player.isDefined) {
    if (player.isEmpty) {
      return lastEscapeVariant
    }

      val onDangerFragment = world.fragments.values.map(f => (f.circle.point.distance(player.get.circle.point) , f)).minBy(_._1)._2

      val escapeVariants = (for (sector <- 0 until angleSectorCount) yield {
        val angle = sector * 2 * Math.PI / (angleSectorCount)
        val escapePoint = new Point(Math.cos(angle), Math.sin(angle)) * predatorEscapeDistance + onDangerFragment.circle.point

        val escapeTrack = Fragment.positionTick(onDangerFragment, escapePoint, world.config)
        val catchTrack = Fragment.positionTick(player.get, escapePoint, world.config)

        val isInterSect = Circle.isIntersectTracks(onDangerFragment.circle, escapeTrack._2, player.get.circle, catchTrack._2, ActionEscape.predatorDiameterFactor)
        (escapePoint, mapCenter.distance(escapePoint), catchTrack._1 - escapeTrack._1, isInterSect)
      }).
        filter(t => t._1.x < world.config.width && t._1.x > 0 && t._1.y < world.config.height)

      if (escapeVariants.filter(!_._4).size > 0) {
        lastEscapeVariant = new Move(escapeVariants.filter(!_._4).maxBy(_._3)._1)
      } else {//damn no escape variants, try do something
        lastEscapeVariant = new Move(escapeVariants.maxBy(_._3)._1)
      }

      lastRunTick = world.tick
    //}

    lastEscapeVariant
  }

  def isEnd(): Boolean = {
    val end = world.players.get(playerId).isEmpty && (world.tick - lastRunTick) > maxRunTick
    end
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: ActionEscape => that.playerId == playerId
      case _ => false
    }

  override def hashCode() : Int = {
    playerId.hashCode
  }
}

object ActionEscape {
  val predatorFactor = 1.2
  val predatorDiameterFactor = 2.0f / 3

  def isDangerAround (world : World) : Option[Action] = {
    if (world.players.size == 0) {
      return None
    }

    val ttfFragments = ActionUnite.ttfFragments(world.fragments)

    val dangerFragments = world.
      fragments.
      values.
      map{
        case f =>
          (
            f
            ,
            math.max(
                  f.ttf.getOrElse(0),
                  Fragment.positionTick(
                    f,
                    ttfFragments.get(f.ttf.getOrElse(0)).get._1,
                    world.config)._1
             )
            ,
            ttfFragments.get(f.ttf.getOrElse(0)).get._2
          )
      }.
      map{
        case (f, ttfSplitTick, ttfWeight) =>
          val dangerPlayers = world.
                players.
                values.
                map(p => (Fragment.positionTick(p, f.circle.point, world.config)._1 , p)).
                filter(p => p._2.weight > f.weight * predatorFactor)

          val needToSplit = dangerPlayers.
                              filter{
                                case (dangerTick, p) =>
                                  (ttfSplitTick < dangerTick && ttfWeight * predatorFactor >= p.weight)
                              }.size > 0

          (f, needToSplit, dangerPlayers)
      }.
      filter(t => t._3.size > 0)

    if (dangerFragments.filter(!_._2).size > 0) {//need escape, split cant safe fragments
      Some(new ActionEscape(dangerFragments.filter(!_._2).flatMap(t => t._3).minBy(_._1)._2.id, world))
    } else if (dangerFragments.filter(_._2).size > 0) {//need split, split safe fragments from enemy
      Some(new ActionUnite(dangerFragments.map(t=> t._1.id), world))
    } else {//no dangerous around
      None
    }
  }
}