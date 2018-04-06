package com.agario.models

import com.agario.utils.Point

import scala.collection.mutable.HashMap

class Track(startTick : Int) {

  val track = HashMap.empty[Int, Step]//include tick, speed and position
  var endTick = 0

  def addStep(tick : Int, step : Step): Unit = {
    endTick = math.max(endTick, tick + startTick)
    track += (startTick + tick -> step)
  }

  def getEndTick (): Int = {
    endTick
  }

  def getStep(step : Int) : Step = {
    track.get(step).get
  }
}


case class Step(val direction : Point, val position : Point)

object Track {
  def empty (): Track = {
    new Track(0)
  }
}