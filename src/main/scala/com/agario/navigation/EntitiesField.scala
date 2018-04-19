package com.agario.navigation

import com.agario.models.{BaseEntity, Food, Fragment, World}
import com.agario.utils.Point

import scala.collection.mutable

class EntitiesField (entitiesType : Set[String], propose : Int = BaseField.defaultPropose, bias : Double = 0f) extends  FadingField(propose, bias){

  val entities = mutable.HashMap[String, BaseEntity]()
  val entitiesCell = mutable.HashMap[String, Point]()
  val cellEntities = mutable.HashMap[Point, mutable.Set[String]]()
  val cellsLastVisibleTick = mutable.HashMap[Point, Int]()
  var visibleCells : Set[Point] = null

  for (i <- 0 to width) {
    for (j <- 0 to height) {
      cellEntities.put(new Point(i, j), mutable.Set())
      cellsLastVisibleTick.put(new Point(i,j), EntitiesField.minVisibleTick)
    }
  }

  def updateEntity(entity : BaseEntity): Unit = {
    if (!entitiesType.contains(entity.eType)) {
      return
    }

    val entityCell = pointCell(entity.posCircle.point)

    if (entities.contains(entity.getId()) && entityCell != entitiesCell.get(entity.getId()).get) {
      cellEntities.get(entitiesCell.get(entity.getId()).get).get.remove(entity.getId())//remove from old cell
    }
    entities.put(entity.getId(), entity)
    entitiesCell.put(entity.getId(), entityCell)

    if (!cellEntities.get(entityCell).get.contains(entity.getId())) {
      cellEntities.get(entityCell).get.add(entity.getId())
    }
  }

  def removeEntity(entityId: String): Unit = {
    if (!entities.contains(entityId)) {
      return;
    }

    val entityCell = entitiesCell.get(entityId).get

    cellEntities.get(entityCell).get.remove(entityId)
    entities.remove(entityId)
    entitiesCell.remove(entityId)
  }

  def setVisibleCells(visibleCells : Set[Point], tick : Int): Unit = {
    this.visibleCells = visibleCells
    visibleCells.foreach(c => cellsLastVisibleTick.put(c, tick))
  }

  def getFactor(fragment : BaseEntity, fragmentPos : Point,  tick : Int) : Map[Point, Double] = {
    val cells = getCircleCells(fragmentPos, fragment.posCircle.r)

    cells.map{
      case cell =>
        val cellLastVisitedTick = cellsLastVisibleTick.get(cell).get
        val duration = (tick - cellLastVisitedTick)
        val factor = {
          if (cellEntities.get(cell).size > 0 && fragment.isInstanceOf[Fragment]) {
            cellEntities.get(cell).get.map {
              case entityId =>
                val entity = entities.get(entityId).get
                val factor = {
                  if (fragment.posCircle.isCover(entity.posCircle, Food.coverPart))
                    entity.factor(fragment.asInstanceOf[Fragment]).values.sum
                  else
                    fadingMinFactor
                }
                getCellFactor(factor, duration)
            }.sum
          } else {
            getCellFactor(fadingMinFactor, duration)
          }
        }

        (cell, factor)
      }.toMap
  }

  def cellFactor(cell: Point, tick : Int): Double = {

    val cellLastVisitedTick = cellsLastVisibleTick.get(cell).get
    val duration = (tick - cellLastVisitedTick)
    val factor = {
      if (cellEntities.get(cell).get.size > 0) {
        cellEntities.get(cell).get.map {
          case entityId =>
            val entity = entities.get(entityId).get
            val factor = entity.factor(World.simpleFragment).values.sum
            getCellFactor(factor, duration)
        }.sum
      } else {
        if (visibleCells.contains(cell)) {
          0.0
        } else {
          getCellFactor(0, duration)
        }
      }
    }
    factor
  }

  def getCellFactor(factor : Double, duration : Double) : Double = {
    if (duration >= 300) {
      fadingMinFactor
    } else {
      fadingMinFactor * (duration) / (1 + duration) + factor / (1 + duration)
    }
  }

  def sum(field : BaseField, tick : Int) : BaseField = {
    val f = new BaseField(propose)
    f.chart ++= field.chart.map{
      case (p, f) =>
        val edgeFactor = if (p.x == 0 || p.y == 0 || p.x == width - 1 || p.y == height - 1) 100 else 0
        (p, f + cellFactor(p, tick) + edgeFactor)
    }
    f
  }
}


object EntitiesField {
  val maxEntityHistory = 1000
  val minVisibleTick = -300
}