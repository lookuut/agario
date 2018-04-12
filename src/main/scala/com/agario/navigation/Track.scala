package com.agario.navigation

import com.agario.utils.Point

import scala.collection.mutable.HashMap

class Track(val startTick : Int) {

  val track = HashMap.empty[Int, Step]//include tick, speed and position
  var endTick = 0
  var endPoint = Point.zero

  def addStep(tick : Int, step : Step): Unit = {
    if (endTick < tick + startTick) {
      endPoint = step.position
      endTick = tick + startTick
    }

    track += (startTick + tick -> step)
  }

  def getEndTick (): Int = {
    endTick
  }

  def getEndPoint(): Point = {
    endPoint
  }

  def duration(): Int = {
    (endTick - startTick)
  }


  def getStep(step : Int) : Step = {
    track.get(step).get
  }
}


case class Step(val direction : Point, val position : Point)

object Track {

  val empty = new Track(0)
}