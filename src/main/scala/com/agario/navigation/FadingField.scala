package com.agario.navigation

import com.agario.models.{Food, World}
import com.agario.utils.Point

class FadingField(world : World, median : Double, propose : Int = FragmentField.defaultPropose)
  extends FragmentField (world, propose) {

  for (i <- 0 to width) {
    for (j <- 0 to height) {
      chart.put(new Point(i, j), median)
    }
  }

  def cellsPowerFading(visibleCells : Set[Point]): Unit = {
    chart.
      filter{
        case (cell, factor) =>
          (
            factor != median
          )
      }.foreach {
      case (p, power) =>
        val f = power / FadingField.fadingFactor

        chart.put(
          p,
          (
            median
              +
              (if (math.abs(f - median) <= FadingField.minMedianDelta) median else f)
          )
        )
    }
  }

  val fadingMinFactor = -(((propose * propose) / (Food.radius * Food.radius * math.Pi)) / (width * height))
  val fadingFactor = fadingMinFactor / FadingField.fadingMaxTick
}


object FadingField {
  val fadingMaxTick = 300f
  val fadingFactor = 1.2f
  val minMedianDelta = 0.000001
  def getStartFadingFactor(propose : Double, width : Int, height : Int) : Double = {
    -(
        (propose * propose) /
            (
              (Food.radius * Food.radius * math.Pi) * (width * height).toDouble * 50
            )
      )
  }

}