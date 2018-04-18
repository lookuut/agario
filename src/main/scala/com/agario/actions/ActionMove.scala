package com.agario.actions

import com.agario.Strategy
import com.agario.commands._
import com.agario.models._
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

    val entities = World.entities.values.
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

    command = searchPath(World.fragments.values, entities, summaryField, World.tick)
    command
  }

  def run () : Command = {
    if (command == null || command.isFinished()) {
      return newCommand()
    }

    if (World.isWorldChanged) {
      if (World.newEntitiesType.contains(BaseEntity.player)) {
          return newCommand()
      }

      if (World.newEntitiesType.contains(BaseEntity.food)) {
        if (command.isInstanceOf[Empty] || command.isInstanceOf[Move]) {
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
            
            //move all fragments to destination point
            val updatedFragments = fragments.map {
              case f =>
                val cF = f.copy()
                val destination = f.destination(track)
                val w = if (f == fragment) entity.weight else 0.0

                cF.update(new Circle(destination._1, f.posCircle.r), destination._2, f.weight + w)
            }
            field.set(Map(field.pointCell(entity.posCircle.point) -> 0))

            val command = {
              if (entity.isInstanceOf[Food]) {
                val lineTrack = ActionMove.searchLines(updatedFragments, field)
                track.append(lineTrack)

                new EatFood(entity, track, World.tick)
              } else if (entity.isInstanceOf[Player]) {
                new Catch(entity, track, World.tick)
              } else if (entity.isInstanceOf[Virus]) {
                new Burst(entity, track, World.tick)
              } else if (entity.isInstanceOf[Ejection]) {
                new Catch(entity, track, World.tick)
              } else {
                new Empty(entity, track, World.tick)
              }
            }

            ((track.trackFactor) / (track.duration() + 1), track, command)
        }

        val track = ActionMove.searchLines(fragments, field)
        val lineTrackFactor = track.trackFactor / (track.duration() + 1)
        if (entityFragments.size == 0) {
          (lineTrackFactor, track, new Move(fragment, track, World.tick))
        } else {
          val minTrack = entityFragments.minBy(_._1)
          if (minTrack._1 < lineTrackFactor) {
            minTrack
          } else {
            (lineTrackFactor, track, new Move(fragment, track, World.tick))
          }
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
            val track = Trajectory.directionTrack(fragment, direction, field, 200)

            (direction, track.duration(), track)
        }
    }.
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