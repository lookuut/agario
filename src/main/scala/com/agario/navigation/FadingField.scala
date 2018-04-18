package com.agario.navigation

import com.agario.models.{Food, World}
import com.agario.utils.{Line, Point}

class FadingField(propose : Int = BaseField.defaultPropose, bias : Double = 0f)
  extends BaseField (propose, bias) {

  val fadingMinFactor = -(((propose * propose) / (Food.radius * Food.radius * math.Pi)) / (width * height))
  val fadingFactor = fadingMinFactor / FadingField.fadingMaxTick

  def setVisibleCells(visibleCells: Set[Point]) : Unit = {
    visibleCells.map{
      case p =>
        chart.put(p, FadingField.visitedCellValue)
    }
  }


  def cellsPowerFading(visibleCells : Set[Point]): Unit = {
    chart.
      filter{
        case (cell, factor) =>
          (
            factor != bias && !visibleCells.contains(cell)
          )
      }.foreach {
      case (p, power) =>
        val f = power / FadingField.fadingFactor

        chart.put(
          p,
          (
            bias
              +
              (if (math.abs(f - bias) <= FadingField.minMedianDelta) bias else f)
          )
        )
    }
  }
}


object FadingField {
  val fadingMaxTick = 300f
  val fadingFactor = 1.015f
  val minMedianDelta = 0.000001
  val visitedCellValue = 0.0

  def getStartFadingFactor(propose : Double, width : Int, height : Int) : Double = {
    -(
        (propose * propose) /
            (
              (Food.radius * Food.radius * math.Pi) * (width * height).toDouble
            )
      )
  }


}