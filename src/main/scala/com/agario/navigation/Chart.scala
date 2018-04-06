package com.agario.navigation

import com.agario.models.World
import com.agario.utils.Point

import scala.collection.mutable.HashMap

class Chart(val world : World) {

  val chart : HashMap[Point, Cell] = HashMap.empty[Point, Cell]

  for (i <- 0 to (world.config.width / Chart.propose).floor.toInt) {
    for (j <- 0 to (world.config.height / Chart.propose).floor.toInt) {
      chart.put(new Point(i, j), new Cell(world))
    }
  }

  def updateChart(world: World): Unit = {

    world.fragments.values.foreach{
      case f =>
        chart.get(pointCell(f.circle.point)).get.addEntity(world.tick, new Entity(Entity.fragment, f.speed, f.circle, f.weight))
    }

    world.players.values.foreach{
      case p =>
        if (p.speed.length() > 0) {
          val futurePoint = p.speed.normalize() * Chart.propose + p.circle.point
          if (futurePoint.x >= 0 && futurePoint.y >= 0 && futurePoint.x < world.chartWidth && futurePoint.y < world.chartHeight) {
            chart.get(pointCell(futurePoint)).get.addEntity(world.tick, new Entity(Entity.player, p.speed, p.circle, p.weight))
          }
        }
        chart.get(pointCell(p.circle.point)).get.addEntity(world.tick, new Entity(Entity.player, p.speed, p.circle, p.weight))
    }
  }


  def pointCell(p : Point): Point = {
    new Point((p.x * (1.0f / Chart.propose)).floor.toInt, (p.y * (1.0f / Chart.propose)).floor.toInt)
  }

  def getCellHistory(p : Point): Cell = {
    chart.get(p).get
  }
}

object Chart {
  val propose = 99
}