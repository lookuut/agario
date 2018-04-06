package com.agario.actions

import com.agario.Strategy
import com.agario.commands.{Command, Move}
import com.agario.models.World
import com.agario.navigation.{Cell, Chart}
import com.agario.utils.Point

class ActionMove(world : World) extends Action {

  val fragmentCells = world.fragments.values.map(f => world.chart.pointCell(f.circle.point)).toSet
  val cells = fragmentCells.map{
    case cell =>
      (-2 to 2).map{
        case i =>
        (-2 to 2).map{
          case j =>
            new Point(cell.x + i, cell.y + j)
        }
      }.
        flatten.
        filter(c => cell != c && c.x >= 0 && c.y >= 0 && c.x < world.chartWidth && c.y < world.chartHeight).
        map(cell => (cell , world.chart.getCellHistory(cell).getMaxTick())).
        filter(t => t._2 < world.tick - Cell.minTickCellHistory || t._2 == 0)
  }.flatten.toArray

  val randomCell = Strategy.rand.nextInt(cells.size - 1)
  val purposeCell = cells(randomCell)
  val point = new Point (purposeCell._1.x * Chart.propose , purposeCell._1.y * Chart.propose) + new Point(Chart.propose / 2, Chart.propose / 2)

  def run () : Command = {
    new Move(point)
  }

  def isEnd(): Boolean = {
    world.fragments.values.filter(f => f.circle.point.distance(point) <= f.circle.r).size > 0
  }
}
