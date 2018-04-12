package com.agario.models

import com.agario.navigation.Track
import com.agario.utils.{Circle, Point, Trajectory}

class Player (
               world: World,
               id : String,
              var posCircle : Circle,
              var speed : Point,
              var weight : Double
            ) extends BaseEntity(id, eType = BaseEntity.player, world)
{

  val playerId = if (id.contains('.')) id.split('.').head.toInt else id.toInt
  val isSplited = id.contains('.')
  val visionRadius = Fragment.visionRadius(this, if (isSplited) 2 else 1)//@todo : get real player's fragments

  override def equals(that: Any): Boolean =
    that match {
      case that: Player => that.id == id
      case _ => false
    }

  override def hashCode() : Int = {
    id.hashCode
  }

  override def factor(fragment: Fragment): (Map[Point, Double], Track) = {

    val factor = Player.factor(fragment, (world.fragments.size >= 2), this, isSplited)

    val track = if (factor < 0)
                  Trajectory.searchTrack(world, fragment, posCircle, Player.coverPart)
                else Track.empty

    world.addVisibleCells(world.
      field.
      getCircleCells(
        this.posCircle.point,
        this.visionRadius
      ).toSet)

    if (factor > 0) {
      val visionRadius = Fragment.visionRadius(this, 1) + Player.playerVisionRadiusDelta//@todo do it well , with player fragments count
      val visionCells = fragment.field.getCircleCells(posCircle.point, visionRadius)
      val visionPoints = fragment.field.cellsCenterToPoints(visionCells)
      val cellTracks = Trajectory.simplePathToPoints(
        world,
        this,
        Player.coverPart,
        visionPoints.map(p => new Circle(p, fragment.posCircle.r))
      )
      val interpolated = cellTracks.map{case (c, track) => (fragment.field.pointCell(c.point), (factor) / math.pow(track.duration() + 1, 2) ) }
      (interpolated, track)
    } else if (factor < 0) {
      val track = Trajectory.searchTrack(world, fragment, this.posCircle, Player.coverPart)
      val visionCells = fragment.field.getCircleCells(posCircle.point, fragment.visionRadius)
      (fragment.field.interpolation(posCircle.point, Point.zero, factor, visionCells), track)
    } else {
      (Map.empty[Point, Double], Track.empty)
    }
  }

  override def fadingFactor(lastFactors : Map[Point, Double]): Map[Point, Double] = {
    lastFactors
  }

  override def isStatic(): Boolean = false
}


object Player {
  val playerStartWeight = 50f
  val playerVisionRadiusDelta = 20
  val coverPart = 2.0f / 3
  val predatorFactor = 1.2f

  val fragmentFactor = 10
  val playerFactor = 100

  def speedByLastPos(currentPos : Point, lastPos : Point) : Point = {
     (currentPos - lastPos)
  }


  def factor (entity1: BaseEntity, entitySplited1 : Boolean, entity2: BaseEntity, entitySplited2 : Boolean): Double = {
    val wk = entity1.weight / entity2.weight

    if (wk >= 1f/predatorFactor && wk <= predatorFactor) {
      return 0f
    } else if (wk < 1f / predatorFactor) {
      if (entitySplited1) {
        fragmentFactor
      } else {
        playerFactor
      }
    } else {
      if (entitySplited2) {
        -fragmentFactor
      } else {
        -playerFactor
      }
    }
  }
}