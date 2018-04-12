package com.agario.navigation

import com.agario.models.World
import com.agario.utils.{Line, Point}

import scala.collection.mutable.{ArrayBuffer, HashMap}

class BaseField (val world : World, val propose : Int) {

  val cellCenter = new Point(propose / 2, propose / 2)
  val width = (world.config.width / propose).floor.toInt
  val height = (world.config.height / propose).floor.toInt

  val chart : HashMap[Point, Double] = HashMap.empty[Point, Double]

  def isEdgeCell(x : Double, y : Double) : Boolean = (x <= 0 || y <= 0 || x >= width - 1 || y >= height - 1)

  def pointFactor (p : Point): Double = {
    val cell = (p * (1.0f / propose)).toInt()

    if (cell.x < 0 || cell.y < 0 || cell.x >= this.width || cell.y >= this.height) {
      FragmentField.maxFieldFactor
    } else {
      cellFactor(cell)
    }
  }

  def cellFactor(cell : Point): Double = {
    chart.get(cell).getOrElse(FragmentField.maxFieldFactor)
  }

  def pointCell(p : Point): Point = {
    val x = (p.x * (1.0f / propose))
    val y = (p.y * (1.0f / propose))

    new Point(if (x < 0) x.ceil.toInt else x.floor.toInt, if (y < 0) y.ceil.toInt else y.floor.toInt)
  }

  def sum(field :  FragmentField) : FragmentField = {
    val f = new FragmentField(world, propose)
    f.chart ++= field.chart.map{case (p, f) => (p, f + this.cellFactor(p))}
    f
  }
  /**
    *
    * @param cell
    * @return world point
    */

  def cellToPoint(cell : Point) : Point = {
    cell * propose
  }

  /**
    *
    * @param cells
    * @return
    */
  def cellsCenterToPoints(cells : Iterable[Point]) : Iterable[Point] = {
    cellsToPoints(cells, cellCenter)
  }

  /**
    *
    * @param cells
    * @return
    */
  def cellsToPoints(cells : Iterable[Point], delta : Point) : Iterable[Point] = {
    cells.map(c => cellToPoint(c) + delta)
  }

  def getCircleCells (center : Point, r : Double): Iterable[Point] = {

    val maxWidth = pointCell(center + new Point(r, 0))
    val minWidth = pointCell(center + new Point(-r, 0))

    val maxHeight = pointCell(center + new Point(0, r))
    val minHeight = pointCell(center + new Point(0, -r))

    (minWidth.x.toInt to maxWidth.x.toInt).map{
      case x =>
        (minHeight.y.toInt to maxHeight.y.toInt).filter{
          case y =>
            val xReal = x * propose
            val yReal = y * propose
            val realCellPoint = new Point(xReal, yReal)
            val distance = realCellPoint.distance(center)

            distance <= r +
              (
                if (xReal < center.x && yReal < center.y)
                  math.sqrt(2) * propose
                else if (xReal < center.x)
                  (1f/math.sqrt(2)) * propose
                else 0f
                ) &&
              x >= 0 && y >= 0 && x < width && y < height
        }.map(y => new Point(x, y))
    }.flatten
  }


  def vectorFactor(startPoint: Point, rDirection : Point): (Double, scala.collection.immutable.Set[Point]) = {

    if (rDirection.length() == 0) {
      return (pointFactor(startPoint), Set(pointCell(startPoint)))
    }
    val visitedCells = scala.collection.mutable.Set[Point](pointCell(startPoint))

    val targetPoint = (startPoint + rDirection).cut(new Point(0,0), new Point(world.config.width - 1, world.config.height - 1) )

    val voxelStartPoint = startPoint  * (1.0 / propose)
    val voxelEndPoint = targetPoint * (1.0 / propose)

    val stepX = propose * (if (rDirection.x < 0) -1
    else 1)
    val stepY = propose * (if (rDirection.y < 0) -1
    else 1)

    var tStartX = (if (stepX >= 0) Math.floor(voxelStartPoint.x).toInt
    else Math.ceil(voxelStartPoint.x).toInt) * propose
    var tStartY = (if (stepY >= 0) Math.floor(voxelStartPoint.y).toInt
    else Math.ceil(voxelStartPoint.y).toInt) * propose

    var previousPoint = startPoint

    val maxWidth = world.config.width
    val maxHeight = world.config.height

    var factorSum = .0
    while ( {
      ((stepX > 0 && Math.floor((tStartX + stepX) / propose) <= Math.floor(voxelEndPoint.x) ||
        stepY > 0 && Math.floor((tStartY + stepY) / propose) <= Math.floor(voxelEndPoint.y)) ||
        (stepX < 0 && Math.floor((tStartX + stepX) / propose) > Math.floor(voxelEndPoint.x) ||
          stepY < 0 && Math.floor((tStartY + stepY) / propose) > Math.floor(voxelEndPoint.y))) &&
        (tStartX + stepX >= 0 && tStartY + stepY >= 0 && tStartX + stepX < maxWidth && tStartY + stepY < maxHeight)
    }) {
      val horIntersectPoint = Line.intersect(startPoint, targetPoint, new Point(0, tStartY + stepY), new Point(maxWidth, tStartY + stepY))
      val verIntersectPoint = Line.intersect(startPoint, targetPoint, new Point(tStartX + stepX, 0), new Point(tStartX + stepX, maxHeight))
      var intersectPoint = targetPoint

      if (horIntersectPoint.isEmpty && verIntersectPoint.isEmpty) {
        throw new Exception("Cant intersect lines found")
      }

      //@TODO workaround
      if (tStartX / propose >= width ||
        tStartY / propose >= height ||
        tStartX / propose < 0 ||
        tStartY / propose < 0) {//todo break
        return (factorSum, visitedCells.toSet)
      }

      if (horIntersectPoint.isDefined && verIntersectPoint.isDefined &&
        (horIntersectPoint.get.length() == verIntersectPoint.get.length())) {
        tStartX += stepX
        tStartY += stepY
        intersectPoint = verIntersectPoint.get

        visitedCells += pointCell(intersectPoint + new Point(0, -math.abs(stepY)))
        visitedCells += pointCell(intersectPoint + new Point(-math.abs(stepX), 0))
        visitedCells += pointCell(intersectPoint + new Point(-math.abs(stepX), -math.abs(stepY)))
      }
      else if (horIntersectPoint.isEmpty ||
        (verIntersectPoint.isDefined &&
          verIntersectPoint.get.distance(startPoint) < (horIntersectPoint.get - startPoint).length)) {
        tStartX += stepX
        intersectPoint = verIntersectPoint.get
        visitedCells += pointCell(intersectPoint + new Point(-math.abs(stepX), 0))
      }
      else if (verIntersectPoint.isEmpty ||
        (horIntersectPoint.isDefined &&
          (verIntersectPoint.get.distance(startPoint) >= horIntersectPoint.get.distance(startPoint)))) {
        tStartY += stepY
        intersectPoint = horIntersectPoint.get
        visitedCells += pointCell(intersectPoint + new Point(0, -math.abs(stepY)))
      }
      visitedCells += pointCell(intersectPoint)
      factorSum += pointFactor(intersectPoint)

      previousPoint = intersectPoint
    }

    if (previousPoint != targetPoint) {
      factorSum += pointFactor(targetPoint)
      visitedCells += pointCell(targetPoint)
    }

    (factorSum, visitedCells.toSet)
  }

  def apply(factors : Map[Point, Double], sign : Int = 1) : Unit = {
    factors.foreach{case (p, f) => chart.put(p, chart.get(p).get + f * sign)}
  }

  def set (factors : Map[Point, Double], sign : Int = 1) : Unit = {
    factors.foreach{case (p, f) => chart.put(p, f * sign)}
  }
}
