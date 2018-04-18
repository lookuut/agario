package com.agario.navigation

import com.agario.models._
import com.agario.utils.Point

import scala.collection._

class WorldMap(entityTypes : Set[String]) {

  val entityCells = mutable.HashMap[String, Point]()
  val entityLastFactors = mutable.HashMap[String, Map[Point, Double]]()

  val field = new BaseField()

  val fadingField = new FadingField(
    BaseField.defaultPropose,
    FadingField.
      getStartFadingFactor(
        BaseField.defaultPropose,
        World.chartWidth,
        World.chartHeight
      )
  )

  val visibleCells = mutable.Set[Point]()

  def addEntity(entity : BaseEntity): Unit = {

    if (!entityTypes.contains(entity.eType)) {
      return
    }

    entityCells.put(entity.getId(), field.pointCell(entity.posCircle.point))
    val factors = entity.factor(World.simpleFragment)//pass to method any fragment
    entityLastFactors += (entity.getId() -> factors)
    field.apply(factors)

    if (entity.eType == BaseEntity.food) {
      val factors = scala.collection.Map(
        field.pointCell(
          new Point(
            entity.posCircle.point.x,
            World.config.height - entity.posCircle.point.y
          )
        ) -> Food.factor,

        field.pointCell(
          new Point(
            World.config.width - entity.posCircle.point.x,
            entity.posCircle.point.y
          )
        ) -> Food.factor,
        field.pointCell(
          new Point(
            World.config.width - entity.posCircle.point.x,
            World.config.height - entity.posCircle.point.y
          )
        ) -> Food.factor
      )

      fadingField.apply(factors)
    }
  }

  def updateEntity(entity: BaseEntity): Unit = {

    if (!entityTypes.contains(entity.eType)) {
      return
    }

    val entityPos = entityCells.get(entity.getId())

    if (!entity.isStatic() && entityPos.get != field.pointCell(entity.posCircle.point)) {
      field.apply(entityLastFactors.get(entity.getId()).get, -1)//remove old factors

      val factors = entity.factor(World.fragments.values.head)//add new
      field.apply(factors)
    }
  }

  def fadeEntity (entity : BaseEntity): Unit = {

    if (!entityCells.contains(entity.getId())) {
      return
    }

    if (World.
      fragments.
      values.
      filter(
        f =>
          f.posCircle.
            isCover(entity.posCircle, Food.coverPart)
      ).size == 0
    ) { //some one eat food, add ejection pos as visible

      visibleCells ++= field.getCircleCells(
            entity.posCircle.point,
            World.lastVisiblePlayer.visionRadius
          )
    }

    entityCells.remove(entity.getId())
    entityLastFactors.remove(entity.getId())
  }

  def addVisibleCells (cells : Set[Point]) : Unit = {
    visibleCells ++= cells
  }

  def setVisibleCells(cells : Set[Point]) : Unit = {
    visibleCells ++= cells

    fadingField.setVisibleCells(visibleCells.toSet)
    fadingField.cellsPowerFading(visibleCells.toSet)

    visibleCells.clear()
  }
}
