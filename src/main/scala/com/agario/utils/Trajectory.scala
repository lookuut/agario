package com.agario.utils

import com.agario.models._

class Trajectory (private val world : World,
                  private val fragment : Fragment,
                  private val target : Circle,
                  private val coverPart : Double) {


  private val maxSpeed =  world.config.speedFactor / math.sqrt(fragment.weight)
  private val factor = world.config.inertionFactor / fragment.weight
  private val line = Point.line(fragment.circle.point, fragment.circle.point + fragment.speed)
  private val targetPosSide = line.pointPos(target.point)
  private var isPassedAngle = false

  var minDistancePoint : Point = fragment.circle.point

  /**
    * Twist fragment to angle relatively speed
    */
  def twist(angle : Double) : (Int, Point, Track)  = {
    if (fragment.speed.length() == 0) {
      return (0, Point.zero(), new Track(world.tick))
    }

    val track = new Track(world.tick)
    val (tick, p) = tracker(fragment.speed, angle, fragment.circle.point, 0, track)

    (tick, p, track)
  }

  def straight (): Track = {
    val track = new Track(world.tick)
    straightTracker(fragment.speed, fragment.circle.point, 0, track)
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
      if (new Circle(newPos, fragment.circle.r).isCover(target, coverPart)) {
        minDistancePoint = newPos
        (tick + 1, minDistancePoint)
      } else {
        tracker(newSpeed, angle, newPos, tick + 1, track)
      }
    } else {
      val normal = newSpeed.normal()
      val speedVecNormalLine = Point.line(newPos + normal, newPos)
      val directionPointSide = speedVecNormalLine.pointPos(newPos + direction)

      if (new Circle(newPos, fragment.circle.r).isCover(target, coverPart)) {
        minDistancePoint = newPos
        (tick + 1, minDistancePoint)
      } else if (speedVecNormalLine.pointPos(target.point) == directionPointSide){
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

    if (!new Circle(newPos, fragment.circle.r).isCover(target, coverPart) || tick <= Trajectory.tickLimit) {
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
  private val tickLimit = 1000
  def searchTrack(world: World, fragment : Fragment, target : Circle, coverPart : Double): Track = {

    var lEdge = math.Pi
    var angle = math.Pi / 4
    var rEdge = 0.0

    val turnAngle = (fragment.circle.point - target.point)
    val atan = math.atan2(turnAngle.y, turnAngle.x)
    val angleSign = if(atan > 0) -1 else 1

    (for (i <- 0 to iterationLimit) yield {
      val trajectory = new Trajectory(world, fragment, target, coverPart)
      val (tick, minDistancePoint, track) = trajectory.twist(angleSign * angle)

      val isCover = new Circle(minDistancePoint, fragment.circle.r).isCover(target, coverPart)

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
}