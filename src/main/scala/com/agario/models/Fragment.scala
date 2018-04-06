package com.agario.models

import com.agario.Config
import com.agario.utils.{Circle, Point}
import scala.collection.mutable.HashMap

class Fragment(id : String,
               circle : Circle,
               weight : Double,
               speed : Point,
               val ttf : Option[Int]) extends Player(id, circle, weight, speed){

  def getId() : String = {
    id
  }

  override def toString() = f"""$id $circle $weight $speed $ttf"""

  override def equals(that: Any): Boolean =
    that match {
      case that: Fragment => that.id == id
      case _ => false
    }

  override def hashCode() : Int = {
    id.hashCode
  }

  def maxSpeed(config : Config): Double = {
    config.speedFactor / math.sqrt(weight)
  }
}

object Fragment {

  def positionOnTick(fragment : Player, tick : Int, direction : Point, config : Config) : Point = {
    val maxSpeed = config.speedFactor / math.sqrt(fragment.weight)
    positionOnTick(tick, fragment.circle.point, direction.normalize(), fragment.weight, fragment.speed, maxSpeed, config)
  }

  def positionTick (fragment: Player, pos : Point, config: Config): (Int, Map[Int, Point]) = {
    val track = HashMap.empty[Int, Point]
    val tick = positionTick(0, pos, fragment.circle.point, fragment, fragment.speed, config.speedFactor / math.sqrt(fragment.weight), config, false, track)

    (tick, track.toMap)
  }

  def moveWithCorrectionTick (fragment: Player, pos : Point, config: Config): (Int, Map[Int, Point]) = {
    val track = scala.collection.mutable.HashMap.empty[Int, Point]
    val tick = positionTick(0, pos, fragment.circle.point, fragment, fragment.speed, config.speedFactor / math.sqrt(fragment.weight), config, true, track)
    (tick, track.toMap)
  }

  def getCorrectionDirect(speed : Point, targetVec : Point, maxSpeed : Double): Point = {

    val angle = speed.angle(targetVec)
    if (speed.length() >= maxSpeed * 0.7 && angle > math.Pi / 20 && angle < math.Pi / 2) {
      //if (angle <= math.Pi / 4) {
        speed.mirrorVector(targetVec).normalize()
      //} else {
        //targetVec.turn(-math.Pi / 4).normalize()
      //}
    } else
      targetVec.normalize()
  }

  def positionTick(tick : Int,
                   targetPos : Point,
                   curPos : Point,
                   fragment: Player,
                   speed : Point,
                   maxSpeed : Double,
                   config : Config,
                   moveCorrection : Boolean = false,
                   track : HashMap[Int, Point]
                  ) : Int = {

    val targetVec = (targetPos - curPos)

    val direction = {
      if (moveCorrection) {
        getCorrectionDirect(speed, targetVec, maxSpeed)
      } else
        targetVec.normalize()
    }

    val newSpeed = tickSpeed(direction, maxSpeed, speed, config.inertionFactor / fragment.weight)
    val newPos = curPos + newSpeed
    track += (tick + 1 -> newPos)

    if ((targetPos - newPos).length() <= fragment.circle.r) {//@TODO fix high speed
      tick + 1
    } else {
      positionTick(tick + 1, targetPos, newPos, fragment, newSpeed, maxSpeed, config, moveCorrection, track)
    }
  }

  def positionOnTick(tick : Int, point : Point, direction : Point, weight : Double, speed : Point, maxSpeed : Double, config : Config) : Point = {

    val newSpeed = tickSpeed(direction, maxSpeed, speed, config.inertionFactor / weight)
    val newPos = point + newSpeed

    if (tick - 1 > 0) {
      positionOnTick(tick - 1, newPos, direction, weight, newSpeed, maxSpeed, config)
    } else {
      newPos
    }
  }

  def tickSpeed (direction : Point, maxSpeed : Double, speed : Point, factor : Double): Point = {
    val x = (direction.x * maxSpeed - speed.x) * factor + speed.x
    val y = (direction.y * maxSpeed - speed.y) * factor + speed.y
    new Point(x, y)
  }

  def maxSpeed(weight : Double, config : Config): Double = {
    config.speedFactor / math.sqrt(weight)
  }

  def radiusByWeight(weight : Double): Double = {
    weight / math.sqrt(weight)
  }
}
