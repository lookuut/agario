package com.agario.utils

import com.agario.models._
import com.agario.navigation.{Step, Track}

class Trajectory (private val world : World,
                  private val fragment : BaseEntity,
                  private val target : Circle,
                  private val coverPart : Double) {


  private val maxSpeed =  world.config.speedFactor / math.sqrt(fragment.weight)
  private val factor = world.config.inertionFactor / fragment.weight
  private var isPassedAngle = false

  var minDistancePoint : Point = fragment.posCircle.point

  /**
    * Twist fragment to angle relatively speed
    */
  def twist(angle : Double) : (Int, Point, Track)  = {
    if (fragment.speed.length() == 0) {
      return (0, Point.zero(), new Track(world.tick))
    }

    if (fragment.posCircle.isCover(target, coverPart)) {
      return (0, target.point, Track.empty())
    }

    val track = new Track(world.tick)
    val (tick, p) = tracker(fragment.speed, angle, fragment.posCircle.point, 0, track)

    (tick, p, track)
  }

  def straight (): Track = {
    val track = new Track(world.tick)

    if (fragment.posCircle.isCover(target, coverPart)) {
      return Track.empty()
    }

    straightTracker(fragment.speed, fragment.posCircle.point, 0, track)
    track
  }

  private def tracker(speed : Point,
                      angle : Double,
                      pos : Point,
                      tick : Int,
                      track : Track
                     ) :  (Int, Point) = {
    val targetPointVec = (target.point - pos).normalize()

    val direction = {
      if (isPassedAngle) {
        targetPointVec
      } else if (targetPointVec.angle(speed) > math.Pi / 100)
        speed.turn(angle).normalize()
      else
        speed.normalize()
    }

    val newSpeed = tickSpeed(direction, speed)

    val oldDirection = if (speed.angleAgainstClockWay(targetPointVec) > 0) 1 else -1
    val newDirection = if (newSpeed.angleAgainstClockWay(targetPointVec) > 0) 1 else -1

    if (oldDirection != newDirection) {
      isPassedAngle = true
    }

    val newPos = pos + newSpeed

    track.addStep(tick + 1, new Step(direction, newPos))

    if (isPassedAngle) {
      if (new Circle(newPos, fragment.posCircle.r).isCover(target, coverPart)) {
        minDistancePoint = newPos
        (tick + 1, minDistancePoint)
      } else {
        tracker(newSpeed, angle, newPos, tick + 1, track)
      }
    } else {
      val normal = newSpeed.normal()
      val speedVecNormalLine = Point.line(newPos + normal, newPos)
      val directionPointSide = speedVecNormalLine.pointPos(newPos + direction)
      val tangentCircle = Line.tangentCircle(pos, speed, newPos, newSpeed)

      if (tick + 1 > Trajectory.tickLimit) {
        (tick + 1, minDistancePoint)
      } else if (new Circle(newPos, fragment.posCircle.r).isCover(target, coverPart)) {
        minDistancePoint = newPos
        (tick + 1, minDistancePoint)
      } else if (speedVecNormalLine.pointPos(target.point) == directionPointSide
        ||
        (tangentCircle.isDefined && !tangentCircle.get.isCoverPoint(target.point))
      ){
        minDistancePoint = newPos
        tracker(newSpeed, angle, newPos, tick + 1, track)
      } else {
        (tick + 1, minDistancePoint)
      }
    }
  }

  private def straightTracker(speed : Point,
    pos : Point,
    tick : Int,
    track : Track
  ) {
    val targetPointVec = (target.point - pos).normalize()

    val newSpeed = tickSpeed(targetPointVec, speed)
    val newPos = pos + newSpeed

    track.addStep(tick + 1, new Step(targetPointVec, newPos))

    if (!new Circle(newPos, fragment.posCircle.r).isCover(target, coverPart) && tick <= Trajectory.tickLimit) {
      straightTracker(newSpeed, newPos, tick + 1, track)
    }
  }

  def tickSpeed (direction : Point, speed : Point): Point = {
    val x = (direction.x * maxSpeed - speed.x) * factor + speed.x
    val y = (direction.y * maxSpeed - speed.y) * factor + speed.y
    new Point(x, y)
  }

  def getPassedAngle (): Boolean = {
    isPassedAngle
  }
}


object Trajectory {
  private val iterationLimit = 100
  private val tickLimit = 300
  def searchTrack(world: World, fragment : BaseEntity, target : Circle, coverPart : Double): Track = {

    if (fragment.speed.length() == 0) {
      return new Trajectory(world, fragment, target, coverPart).straight()
    }
    var lEdge = math.Pi
    var angle = math.Pi / 4
    var rEdge = 0.0

    val targetDirection = (target.point - fragment.posCircle.point)

    val angleSign = if (fragment.speed.angleAgainstClockWay(targetDirection) > 0) 1 else -1

    (for (i <- 0 to iterationLimit) yield {
      val trajectory = new Trajectory(world, fragment, target, coverPart)
      val (tick, minDistancePoint, track) = trajectory.twist(angleSign * angle)

      val isCover = new Circle(minDistancePoint, fragment.posCircle.r).isCover(target, coverPart)

      if (!trajectory.getPassedAngle()) {//flight over
        rEdge = angle
      } else {
        lEdge = angle
      }
      angle = (lEdge + rEdge) / 2

      if (isCover)
        (i, tick, track)
      else
        (i, tickLimit, new Track(world.tick))

    }).
      minBy(_._2)._3
  }

  def simplePathToPoints(world : World, entity: BaseEntity, coverPart : Double, targets : Iterable[Circle]) : Map[Circle, Track] = {
    targets.map {
      case c =>
        (c, new Trajectory(world, entity, c, coverPart).straight())
    }.toMap
  }
}