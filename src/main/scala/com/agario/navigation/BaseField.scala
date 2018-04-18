package com.agario.navigation

import com.agario.Consts
import com.agario.models.World
import com.agario.utils.{Line, Point}

import scala.collection.mutable.HashMap

class BaseField (val propose : Int = BaseField.defaultPropose, val bias : Double = 0f) {
  val oppPropose = 1.0 / propose

  val cellCenter = new Point(propose / 2, propose / 2)
  val width = (World.config.width * oppPropose).floor.toInt
  val height = (World.config.height * oppPropose).floor.toInt

  val chart : HashMap[Point, Double] = HashMap.empty[Point, Double]

  for (i <- 0 to width) {
    for (j <- 0 to height) {
      chart.put(new Point(i, j), bias)
    }
  }

  def getFactors(cells : Set[Point]) : Map[Point, Double] = {
    cells.filter(p => chart.contains(p)).map{
      case p => (p,chart.get(p).get)
    }.toMap
  }

  def getFactors(cells : Set[Point], startPoint : Point, direction : Point): Map[Point, Double] = {

    val (start, length) = {
      if (direction.y < 0) {
        (0.0, startPoint.y)
      } else if (direction.y > 0) {
        (startPoint.y, height.toDouble)
      } else if (direction.x < 0) {
        (0.0, startPoint.x)
      } else {
        (startPoint.x, width.toDouble)
      }
    }

    val startX = if(direction.x != 0) start else 0.0
    val endX = if(direction.x != 0) length else width.toDouble

    val startY = if(direction.y != 0) start else 0.0
    val endY = if(direction.y != 0) length else height.toDouble

    chart.filter{
      case (p, v) =>
        (
          (p.x >= startX && p.x <= endX && p.y >= startY && p.y <= endY)
            &&
          (cells.contains(p))
        )
    }.toMap
  }

  def interpolation(center : Point, speed : Point, factor : Double, cells : Iterable[Point]) : Map[Point, Double] = {
    val cCenter = pointCell(center)
    cells.map {
      case p =>
        val point = cCenter - new Point(p.x, p.y)

        val speedCell = pointCell(speed * ((BaseField.speedPower - World.config.inertionFactor) / 5) )
        val fieldCell = speedCell + p
        (fieldCell, factor / (speedCell.length() + 1 + point.length()))
    }.
      filter{case(p,f) => p.x >= 0 && p.y >= 0 && p.x < width && p.y < height}.
      toMap
  }

  def isEdgeCell(x : Double, y : Double) : Boolean = (x <= 0 || y <= 0 || x >= width - 1 || y >= height - 1)

  def pointFactor (p : Point): Double = {
    val cell = (p * oppPropose).toInt()

    if (cell.x < 0 || cell.y < 0 || cell.x >= this.width || cell.y >= this.height) {
      BaseField.maxFieldFactor
    } else {
      cellFactor(cell)
    }
  }

  def cellFactor(cell : Point): Double = {
    chart.get(cell).getOrElse(BaseField.maxFieldFactor)
  }

  def pointCell(p : Point): Point = {
    val x = (p.x * oppPropose)
    val y = (p.y * oppPropose)

    new Point(if (x < 0) x.ceil.toInt else x.floor.toInt, if (y < 0) y.ceil.toInt else y.floor.toInt)
  }

  def sum(field :  BaseField) : BaseField = {
    val f = new BaseField(propose)
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

  def getMaxFactor (cells : Iterable[Point]) : (Double, Point) = {
    if (cells.size == 0) {
      return (0.0 , Point.zero)
    }

    cells.map {
      p =>
        (cellFactor(p), p)
    }.maxBy(_._1)
  }

  def getCircleCells (center : Point, r : Double): Set[Point] = {

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
                  Consts.sqrt2 * propose
                else if (xReal < center.x)
                  Consts.oppSqrt2 * propose
                else 0f
                ) &&
              x >= 0 && y >= 0 && x < width && y < height
        }.map{
          case y =>
            new Point(x, y)
        }
    }.flatten.toSet
  }


  def vectorFactor(startPoint: Point, rDirection : Point): (Double, scala.collection.immutable.Set[Point]) = {

    if (rDirection.length() == 0) {
      return (pointFactor(startPoint), Set(pointCell(startPoint)))
    }
    val visitedCells = scala.collection.mutable.Set[Point](pointCell(startPoint))

    val targetPoint = (startPoint + rDirection).cut(new Point(0,0), new Point(World.config.width - 1, World.config.height - 1) )

    val voxelStartPoint = startPoint  * oppPropose
    val voxelEndPoint = targetPoint * oppPropose

    val stepX = propose * (if (rDirection.x < 0) -1
    else 1)
    val stepY = propose * (if (rDirection.y < 0) -1
    else 1)

    var tStartX = (if (stepX >= 0) Math.floor(voxelStartPoint.x).toInt
    else Math.ceil(voxelStartPoint.x).toInt) * propose
    var tStartY = (if (stepY >= 0) Math.floor(voxelStartPoint.y).toInt
    else Math.ceil(voxelStartPoint.y).toInt) * propose

    var previousPoint = startPoint

    val maxWidth = World.config.width
    val maxHeight = World.config.height

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

  def apply(factors : scala.collection.Map[Point, Double], sign : Int = 1) : Unit = {
    factors.foreach{case (p, f) => if (chart.contains(p)) chart.put(p, chart.get(p).get + f * sign)}
  }

  def set (factors : scala.collection.Map[Point, Double]) : Unit = {
    factors.foreach{case (p, f) => if (chart.contains(p)) chart.put(p, f)}
  }


  override def toString: String = {
    try {

      (0 until height).map({
        case j =>
          (0 until width).map({
            case i =>
              val cellPower = chart.get(new Point(i,j)).get.toString
              cellPower
          }).mkString(",")
      }).mkString("\n")
    }catch {
      case e : Exception => e.getMessage
    }

  }
}


object BaseField {
  val defaultPropose = 30//change it
  val fragmentPower = 1f
  val playerFieldRadius = 5
  val speedPower = 25.0f
  val minFieldFactor = 0.000001
  val maxFieldFactor = Double.PositiveInfinity
  val edgeFactor =  0.0
  val empty = new BaseField(World.config.width)

  def transform(cells : Map[Point, Double], vector : Point) : Map[Point, Double] = {
    cells.map{
      case (p, f) =>
        (p + vector, f)
    }
  }

  def mirrorCells (cells : Map[Point, Double], center : Point, direction : Point): Map[Point, Double] = {

    val centerCell = center + (if (direction.x > 0) new Point(-1, 0) else Point.zero) + (if (direction.y > 0) new Point(0, -1) else Point.zero)
    cells.filter{
      case (p, f) =>
        (direction.x > 0 && p.x >= centerCell.x ||
          direction.y > 0 && p.y >= centerCell.y ||
          direction.y < 0 && p.y <= centerCell.y ||
          direction.x < 0 && p.x <= centerCell.x )
    }.map{
      case (p , f) =>
        val point = {
          if (direction.y != 0)
            new Point(p.x, centerCell.y - (p.y - centerCell.y))
          else
            new Point(centerCell.x - (p.x - centerCell.x), p.y)
        }

        (point, f)
    }
  }

  def pointCell(point : Point, propose : Double = defaultPropose): Point = {
    new Point((point.x / propose).floor.toInt, (point.y / propose).floor.toInt)
  }
}