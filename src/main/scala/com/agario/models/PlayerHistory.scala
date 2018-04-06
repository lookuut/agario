package com.agario.models

import com.agario.utils.Point

import scala.collection.mutable.HashMap

class PlayerHistory (world: World) {
  val history = HashMap.empty[Int, Map[String, Point]]

  def addHistory (tick: Int , players : Map[String, Player]): Unit = {
    if (players.size > 0) {
      history += (tick -> players.map{case (id, player) => (id, player.circle.point)})
    }

    clearHistory()
  }

  def clearHistory(): Unit = {
    history --= history.filter{case (tick, history) => tick < tick - historyMinTick}.map(_._1)
  }

  def getPlayerHistory(tick : Int, playerId : String) : Option[Point] = {
    val pHistory = history.get(tick)
    if (pHistory.isEmpty) {
      return None
    }

    pHistory.get.get(playerId)
  }

  private val historyMinTick = 50
}
