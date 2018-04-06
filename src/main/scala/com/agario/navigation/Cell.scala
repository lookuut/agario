package com.agario.navigation

import com.agario.models.World

import scala.collection.mutable.{ArrayBuffer, HashMap}

class Cell(val world : World) {

  val history = HashMap.empty[Int, ArrayBuffer[Entity]]
  var maxTick = 0

  def addEntity(tick : Int, entity : Entity): Unit = {
    maxTick = tick
    if (history.contains(tick)) {
      history.get(tick).get += entity
    } else {
      val arrayBuffer = new ArrayBuffer[Entity]()
      arrayBuffer += entity
      history.put(tick, arrayBuffer)
    }
  }

  def getMaxTick(): Int = {
    maxTick
  }

  def clear(): Unit = {
    history --= history.keys.filter{tick => tick > world.tick - Cell.minTickCellHistory}
  }
}


object Cell {
  val minTickCellHistory = 400
}