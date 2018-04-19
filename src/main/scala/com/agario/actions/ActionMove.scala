package com.agario.actions

import com.agario.Strategy
import com.agario.commands._
import com.agario.models.{World, _}
import com.agario.navigation.{BaseField, Track}
import com.agario.utils._

case class TrackDuration(val cells : Set[Point], val duration: Int)

/**
  * @todo search minimal track on graph
  */

class ActionMove extends Action {

  var command : Command = null

  val reduceTuple : ((Double, Int), (Double, Int)) => (Double, Int) = (l, r) => {
    (l._1 + r._1, l._2 + r._2)
  }

  def newCommand(searchFood : Boolean = false): Command = {
    val summaryField = World.getFieldsSum()

    val dangerFragment = World.fragments.values.
      filter(f => summaryField.getFactors(Set(f.fragmentCell)).get(f.fragmentCell).get > 0)
    if (dangerFragment.size > 0){
      command = new Escape(dangerFragment.head, if (World.players.values.size > 0) World.players.values.head else World.fragments.values.head , Track.empty, World.tick)
      return command
    }

    val entities =
      if (World.fragments.size >= 2 || World.fragments.size == 0 || World.entities.size == 0)
        World.players.values
      else {
        World.entities.values.
          map{
            case e =>
              val sum = World.fragments.values.map{
                case f =>
                  val distance = e.posCircle.point.distance(f.posCircle.point)
                  val predictions = Strategy.getPrediction(f.weight, Strategy.getPosId(f.posCircle.point, f.speed), f.speed)
                  val tick =
                    if (predictions.length == 0)
                      Strategy.getPredictionMinTick(f.speed, predictions)._1
                    else
                      math.ceil(if (f.speed.length() > 0) distance / f.speed.length() else distance / (f.maxSpeed / 2)).toInt

                  (e.factorValue(f), tick)
              }.reduce(reduceTuple)

              (e, sum._1 / World.fragments.size , sum._2 / World.fragments.size)
          }.
          toSeq.sortBy(t => (t._3, t._2)).take(5).map(_._1)
      }

    if (entities.size == 0) {
      val track = ActionMove.searchLines(World.fragments.values, summaryField)
      command = new Move(World.fragments.values.head, World.fragments.values.head, track, World.tick)
    } else {
      command = searchPath(World.fragments.values, entities, summaryField, World.tick)
    }

    command
  }

  def run () : Command = {
    if (command == null || command.isFinished()) {
      return newCommand()
    }

    if (command.isInstanceOf[Escape] && !command.isFinished()) {
      return command
    } else if (World.isWorldChanged) {
      if (World.newEntitiesType.contains(BaseEntity.player)) {
          return newCommand()
      }

      if (World.newEntitiesType.contains(BaseEntity.food) && World.fragments.size < 2) {
        if (command.isInstanceOf[Empty] || command.isInstanceOf[Move] || command.isInstanceOf[EatFood]) {
          return newCommand(true)
        }
      }
    }

    return command
  }

  def searchPath (
                   fragments : Iterable[Fragment],
                   entities : Iterable[BaseEntity],
                   field : BaseField,
                   startTick : Int
                 ) : Command = {
    val optimalTracks = fragments.map {
      case fragment =>
        val entityFragments = entities.filter(e => e.canEat(fragment)).map {
          case entity =>
            val track = Trajectory.optimalTrack(fragment, entity.posCircle, field)

            val command = {
              if (entity.isInstanceOf[Food]) {
                new EatFood(fragment, entity, track, World.tick)
              } else if (entity.isInstanceOf[Player]) {
                new Catch(fragment, entity, track, World.tick)
              } else if (entity.isInstanceOf[Virus]) {
                new Burst(fragment, entity, track, World.tick)
              } else if (entity.isInstanceOf[Ejection]) {
                new Catch(fragment, entity, track, World.tick)
              } else {
                new Empty(fragment, entity, track, World.tick)
              }
            }

            ((track.trackFactor) / (track.duration()), track, command)
        }.filter(t => t._2.duration() > 0 && t._1 < 0)

        if (entityFragments.size == 0) {
          val track = ActionMove.searchLines(fragments, field)
          val lineTrackFactor = track.trackFactor / (track.duration())
          (lineTrackFactor, track, new Move(fragment, fragment, track, World.tick))
        } else {
          val minTrack = entityFragments.minBy(_._1)
          minTrack
        }
    }

    optimalTracks.minBy(_._1)._3
  }

  def isEnd(): Boolean = {
    command.isFinished()
  }
}


object ActionMove {
  val sectorsCount = 8
  val direction = new Point(1, 0)
  val sectors = (0 to sectorsCount).map{
    case sector =>
      val angle = sector.toDouble * 2f * math.Pi / sectorsCount
      Strategy.xDir.turn(angle)
  }


  def searchLines(fragments : Iterable[BaseEntity], field : BaseField): Track = {

    val optimalTrack = fragments.flatMap {
      case fragment =>
        ActionMove.sectors.map {
          case direction =>
            val track = Trajectory.directionTrack(fragment, direction, field, 80)

            (direction, track.duration(), track)
        }
    }.filter(_._3.duration() > 0).
      groupBy(_._1).
      map{
        case (dir, factors) =>
          val reduceDuration : (TrackDuration, TrackDuration) => TrackDuration = (l, r) => {
            val res = l.cells ++ r.cells
            new TrackDuration(res, if (l.cells.size < res.size) r.duration else l.duration)
          }
          val directionCells = factors.
            toSeq.
            sortBy(_._2).//sort by duration asc
            map(t => new TrackDuration(t._3.visitedCells.keys.toSet, t._2)).
            reduce(reduceDuration)

          val directionDuration = directionCells.duration
          val track = factors.map(t => t._3).filter(t => t.duration() == directionDuration).head
          val directionFactor = field.getFactors(directionCells.cells).values.sum
          (dir,  directionFactor , track)
      }.
      minBy(_._2)

    optimalTrack._3
  }
}