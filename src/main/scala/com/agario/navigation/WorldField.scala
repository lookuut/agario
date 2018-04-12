package com.agario.navigation

import com.agario.models.World
import com.agario.utils.Point

class WorldField(world : World, edgeValue : Double, median : Double, propose : Int = FragmentField.defaultPropose)
  extends FadingField (world, propose) {

  for (i <- 0 to width) {
    for (j <- 0 to height) {
      val factor = if (isEdgeCell(i, j)) edgeValue else median
      chart.put(new Point(i, j), factor)
    }
  }

  override def cellsPowerFading(visibleCells : Set[Point]): Unit = {
    chart.
      filter{
        case (cell, factor) =>
          (
            !visibleCells.contains(cell)
              &&
              !isEdgeCell(cell.x, cell.y)
              &&
              factor != median
            )
      }.foreach {
      case (p, power) =>
        chart.put(p, if (power > median)
          (power + fadingFactor)
        else
          (power + math.abs(fadingFactor))
        )
    }

    visibleCells.filter(p => !isEdgeCell(p.x, p.y)).
      foreach {
      case p =>
        chart.put(p, 0)
    }
  }
}
