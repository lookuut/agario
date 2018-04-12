package com.agario.navigation

import com.agario.models._
import com.agario.utils.{Point}

class FragmentField(world : World, propose : Int = FragmentField.defaultPropose)
  extends BaseField (world , propose) {

  for (i <- 0 to width) {
    for (j <- 0 to height) {
      chart.put(new Point(i, j), 0)
    }
  }

  def interpolation(center : Point, speed : Point, factor : Double, cells : Iterable[Point]) : Map[Point, Double] = {
    val cCenter = pointCell(center)
    cells.map {
      case p =>
      val point = cCenter - new Point(p.x, p.y)

      val speedCell = pointCell(speed * ((FragmentField.speedPower - world.config.inertionFactor) / 5) )
      val fieldCell = speedCell + p
      (fieldCell, factor / math.pow((speedCell.length() + 1 + point.length()), 2))
    }.
      filter{case(p,f) => p.x >= 0 && p.y >= 0 && p.x < width && p.y < height}.
      toMap
  }


  override def toString: String = {
    try {

      (0 until height).map({
        case j =>
        (0 until width).map({
          case i =>
            val cellPower = chart.get(new Point(i,j)).get.toString
            cellPower
          /*

            val strCellPower = {
              if (cellPower < 0.00001f && cellPower >= 0f) {
                0f
              } else {
                cellPower
              }
            }.toString

            if (strCellPower.length < 6) {
              strCellPower + (0 to 6 - strCellPower.length).map(t => "0").mkString("")
            } else {
              strCellPower.substring(0, 7)
            }*/

        }).mkString(",")
    }).mkString("\n")
    }catch {
      case e : Exception => e.getMessage
    }

  }
}

object FragmentField {
  val defaultPropose = 15//change it
  val fragmentPower = 1f
  val playerFieldRadius = 5
  val speedPower = 25.0f
  val minFieldFactor = 0.000001
  val maxFieldFactor = Double.PositiveInfinity
  val edgeFactor =  0.01
}