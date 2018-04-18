package com.agario.utils

import com.agario.Strategy
import com.agario.models._
import com.agario.navigation.{BaseField, Step, Track}

class Trajectory (
                   private val fragment : BaseEntity,
                   private val target : Circle,
                   private val coverPart : Double,
                   private val field : BaseField
                 ) {

  def tickSpeed (direction : Point, speed : Point): Point = {
    Trajectory.tickSpeed(direction, speed, fragment.maxSpeed, fragment.inertion)
  }
}


object Trajectory {
  val iterationLimit = 100
  val tickLimit = 300
  val straightTickLimit = 500
  val searchLineTickLimit = 100

  private val noCoverPenalty = 1.0

  def tickSpeed (direction : Point, speed : Point, maxSpeed : Double, inertion : Double): Point = {
    new Point(
      (direction.x * maxSpeed - speed.x) * inertion + speed.x,
      (direction.y * maxSpeed - speed.y) * inertion + speed.y
    )
  }

  def speed(sSpeed : Point, dir : Point, maxSpeed : Double, inertia : Double, n : Int): Point = {

    var k = 0.0
    for (i <- 0 to n - 1) {
      k += math.pow(1- inertia, i)
    }

    (sSpeed * math.pow(1- inertia, n)) + (dir * (maxSpeed * inertia * k))
  }

  def directionTrack(fragment: BaseEntity, dir : Point, field : BaseField, tickLimit : Int) : Track = {

    val track = new Track(field)

    var pos = fragment.posCircle.point
    var speed = fragment.speed

    for (step <- 0 to tickLimit) {
      speed = tickSpeed(dir, speed, fragment.maxSpeed, fragment.inertion)
      pos += speed
      if (pos.x + fragment.posCircle.r > World.config.width ||
        pos.y + fragment.posCircle.r > World.config.height ||
        pos.x < fragment.posCircle.r ||
        pos.y < + fragment.posCircle.r) {

        return track
      }

      track.addStep(step + 1, new Step(dir, pos), getStepFactors(pos, step + 1, fragment, field))
    }

    track
  }

  def optimalTrack(fragment: BaseEntity, target : Circle, field : BaseField) : Track = {
    val (tick, dir) = optimalDirection(fragment.speed, fragment.inertion, fragment.maxSpeed, target.point - fragment.posCircle.point)

    val track = new Track(field)

    var pos = fragment.posCircle.point
    var speed = fragment.speed
    for (step <- 0 to tick) {
      speed = tickSpeed(dir, speed, fragment.maxSpeed, fragment.inertion)
      pos += speed
      track.addStep(step + 1, new Step(dir, pos), getStepFactors(pos, step + 1, fragment, field))
    }

    track
  }

  def optimalDirection(sSpeed : Point, inertia : Double, mSpeed : Double, target : Point) : (Int, Point) = {
    val I = (1 - inertia)
    val M = inertia * mSpeed

    for (n <- 0 to 200) {

      var sSpeedFactorSum = 0.0
      for (i <- 1 to n) {
        sSpeedFactorSum += math.pow(I, n)
      }

      val sSpeedFactor = sSpeed * sSpeedFactorSum

      var mSpeedFactor = 0.0
      for (i <- 1 to n) {
        for (j <- 0 to i - 1) {
          mSpeedFactor += math.pow(I, j)
        }
      }
      mSpeedFactor = mSpeedFactor * M
      if (mSpeedFactor > 0) {
        val x = (target.x - sSpeedFactor.x) / mSpeedFactor
        val y = (target.y - sSpeedFactor.y) / mSpeedFactor
        val direction = new Point(x, y)

        if (direction.length() <= 1) {
          return (n, direction)
        }
      }
    }

    return (0, Point.zero)
  }

  def getStepFactors(pos : Point, tick : Int, fragment: BaseEntity, field: BaseField) : Map[Point, Double] = {
    val cells = field.getCircleCells(pos, fragment.posCircle.r)
    World.staticEntitiesField.getFactor(fragment, pos, tick + World.tick) ++ field.getFactors(cells)
  }

  def playerDangerField(entity: BaseEntity, coverPart : Double, targets : Iterable[Point], factor : Double): Map[Point, Double] = {

    targets.map{
      case cell =>
        val pos = World.staticEntitiesField.cellToPoint(cell) - entity.posCircle.point
        val tick = optimalDirection(entity.speed, entity.inertion, entity.maxSpeed, pos)._1
        (cell, factor / tick)
    }.toMap
  }
}