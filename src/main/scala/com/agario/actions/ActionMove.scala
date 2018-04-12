package com.agario.actions

import com.agario.commands.{Command, Move}
import com.agario.models.{Fragment, World}
import com.agario.navigation.FragmentField
import com.agario.utils.Point

/**
  * @todo search minimal track on graph
  * @param world
  */

class ActionMove(world : World) extends Action {


  val summaryField = world.
    fragments.
    values.map(f => f.getFieldsSum()).reduce((f1, f2) => f1.sum(f2)).
    sum(world.field)

  val minFactorDirection = world.fragments.values.map {
    case fragment =>
      ActionMove.sectors(fragment.visionRadius + FragmentField.defaultPropose).map{
        case direction =>
          val factor = summaryField.vectorFactor(fragment.posCircle.point, direction)
          (direction, factor)
      }
  }.
    flatten.
    groupBy(_._1).
    map{
      case (dir, factors) =>
        (dir,  factors.map(_._2._1).sum / factors.size, factors.head._2._2)
    }.
    minBy(_._2)

  val direction = minFactorDirection._1
  val minFactorCells = minFactorDirection._3

  val tracks = world.fragments.values.map{
    case fragment =>
      minFactorCells.map(
        cell => fragment.
          tracks.
          get(
            cell - fragment.fragmentCell
          )
      )
  }.flatten.
    filter{
    case t => t.isDefined && t.get.endTick > world.tick
  }.map {
    case t => (t.get.endTick, t.get)
  }

  val fragment = world.fragments.head._2
  val optimalTrack = if (tracks.size > 0) Some(tracks.minBy(_._1)._2) else None
  val point = if (optimalTrack.isDefined)
                optimalTrack.get.getEndPoint()
              else
                (
                    fragment.posCircle.point
                      +
                    direction
                  )

  def run () : Command = {
    new Move(point, optimalTrack)
  }

  def isEnd(): Boolean = {
    if (optimalTrack.isDefined)
      !(optimalTrack.get.endTick > world.tick)
    else
      world.fragments.values.filter(f => f.posCircle.point.distance(point) <= f.posCircle.r).size > 0
  }
}


object ActionMove {
  val sectorsCount = 24
  val direction = new Point(1, 0)
  val maxPathLenght = 70

  def sectors(lenght : Double) : Iterable[Point] = {
    (0 to sectorsCount).map{
      case sector =>
        val angle = sector.toDouble * 2f * math.Pi / sectorsCount
        direction.turn(angle) * lenght
    }
  }
}