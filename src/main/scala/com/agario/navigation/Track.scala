package com.agario.navigation

import com.agario.utils.{Circle, Point}

import scala.collection.mutable.HashMap

class Track(field : BaseField) {

  val track = HashMap.empty[Int, Step]//include tick, speed and position
  var endStepNumber = 0
  var endStepPoint = Point.zero
  var lastStep = new Step(Point.zero, Point.zero)
  val visitedCells = scala.collection.mutable.HashMap[Point, Double]()
  var trackFactor = 0.0

  //@todo intersect circle and square
  def addStep(stepNumber : Int, step : Step, factorCells : Map[Point , Double]): Unit = {
    if (endStepNumber < stepNumber) {
      endStepPoint = step.position
      endStepNumber = stepNumber
      lastStep = step
    }

    val cell = BaseField.pointCell(step.position)
    if (!visitedCells.contains(cell)) {

      visitedCells ++= factorCells
      trackFactor += factorCells.values.sum
    }

    track += (stepNumber -> step)
  }

  def getEndTick (): Int = {
    endStepNumber
  }

  def getEndPoint(): Point = {
    endStepPoint
  }

  def duration(): Int = {
    endStepNumber
  }

  def getStep(step : Int) : Option[Step] = {
    track.get(step)
  }

  def getLastStep(): Step = {
    lastStep
  }

  def append(that : Track) : Unit = {
    that.track.foreach {
      case(stepNumber, step) =>
        track.put(stepNumber + duration(), step)
    }
    endStepNumber = duration() + that.duration()
    endStepPoint = that.endStepPoint
    lastStep = that.lastStep
    visitedCells ++= that.visitedCells
    trackFactor += that.trackFactor
  }
}


case class Step(val direction : Point, val position : Point)

object Track {
  val empty = new Track(BaseField.empty)
}