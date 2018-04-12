package com.agario.models

import com.agario.Config
import com.agario.navigation.{BaseField, FadingField, FragmentField, WorldField}
import com.agario.utils.{Circle, Point}

import scala.collection.mutable.Set

class World(val config : Config) {

  val chartWidth = (config.width / FragmentField.defaultPropose).floor.toInt
  val chartHeight = (config.height / FragmentField.defaultPropose).floor.toInt

  val mapCenter = new Point(config.width / 2, config.height / 2)
  var tick = 0

  var entityPositions = scala.collection.immutable.Set[Point]()
  var entityPrevStates = Map.empty[String, BaseEntity]
  var fragments = Map.empty[String, Fragment]
  var entities = Map.empty[String, BaseEntity]
  var players =  Map.empty[String, Player]

  val field = new WorldField(
    this,
    FragmentField.edgeFactor,
    FadingField.
      getStartFadingFactor(
        FragmentField.defaultPropose,
        chartWidth,
        chartHeight)
  )

  val visibleCells = Set[Point]()

  var isWorldChanged = false

  var lastVisiblePlayer = new Player(this,
      "-1",
      new Circle(
        new Point(-1, 1),
        Fragment.radiusByWeight(Player.playerStartWeight)
      ),
      Point.zero(),
    Player.playerStartWeight
  )

  def updateWorld (fEntities : Array[Entity],
                   eEntities : Array[Entity],
                   _tick : Int
                    ): Unit = {

    visibleCells.clear()
    entityPrevStates = entities

    fragments = fEntities.map{
      case e =>
        if (fragments.contains(e.getId())) {
          val updatedFragment = getFragment(e.getId()).
                                  update(new Circle(e.point, e.r.get), e.speed, e.weight.get, e.ttf.getOrElse(0))
          (
            e.getId(),
            updatedFragment
          )
        } else {
          (e.getId(), e.fragment(this))
        }
    }.toMap

    entities = eEntities.map{
      case e =>
        val newEntity = e.getEntity(this)
        if (entities.contains(e.getId())) {
          val entity = entities.
            get(e.getId()).
            get

          (
            e.getId(),
            if (entity.isInstanceOf[Fragment]) {
              entity.asInstanceOf[Fragment].update(newEntity.posCircle, newEntity.speed, newEntity.weight, newEntity.asInstanceOf[Fragment].ttf)
            } else {
              entity.update(newEntity.posCircle, newEntity.speed, newEntity.weight)
            }
          )
        } else {
          (e.getId(), e.getEntity(this))
        }
    }.toMap

    players = entities.
      filter{case (id, e) => e.eType == BaseEntity.player}.
      map{case (id, player)  => (id, player.asInstanceOf[Player])}

    lastVisiblePlayer = if (players.size > 0) players.values.head else lastVisiblePlayer

    val fragment = fragments.values.head
    val curEntityPositions = entities.values.map(e => fragment.field.pointCell(e.posCircle.point)).toSet

    this.isWorldChanged = curEntityPositions != entityPositions
    this.entityPositions = curEntityPositions
    tick = _tick

    fragments.values.foreach{
      case f =>
        if (isWorldChanged) {

          f.updateMapEntities()
        }

        visibleCells ++= field.getCircleCells(f.visionCenter(), f.visionRadius)//@todo do it smarter
        f.fadeField()
    }

    field.cellsPowerFading(visibleCells.toSet)
  }

  def addVisibleCells(cells : scala.collection.immutable.Set[Point]) : Unit = {
    visibleCells ++= cells
  }

  var maxRadiusFragment : Option[Fragment] = None
  var maxRadiusFragmentCount = 0

  def getMaxRadius(): Double = {
    if (maxRadiusFragmentCount != fragments.size && fragments.size > 0) {
      maxRadiusFragmentCount = fragments.size
      maxRadiusFragment = Some(fragments.values.map(f => (f.posCircle.r, f)).maxBy(_._1)._2)
    }

    if (maxRadiusFragment.isDefined) {
      maxRadiusFragment.get.posCircle.r
    } else {
      0.0
    }
  }

  def getMinDistanceFragment(point : Point): Option[Fragment] = {

    if (fragments.size == 0) {
      return None
    }

    Some(fragments.values.map(f => (f.posCircle.point.distance(point), f)).minBy(_._1)._2)
  }

  def getFragment(id : String) : Fragment = {
    fragments.get(id).get
  }

  def getPlayer(id : String) : Option[BaseEntity] = {
    entities.get(id)
  }

  def getEntities(etype : String): Map[String, BaseEntity] = {
    entities.filter{case(id, e) => e.eType == etype}
  }

  def getPlayers (): Iterable[Player] = {
    players.values
  }

  def getEntity(id : String) : Option[BaseEntity] = {
    entities.get(id)
  }

  def getPlayerIdEntities(playerId : Int): Iterable[BaseEntity] = {
    players.values.filter(e => e.playerId == playerId)
  }

  def getFragmentFieldSum(): BaseField = {
    fragments.
      values.
      map(t => t.getFieldsSum()).
      reduce{(left, right) => left.sum(right)}.sum(field)
  }
}