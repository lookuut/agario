package com.agario.models

import com.agario.Config
import com.agario.navigation.{BaseField, EntitiesField, FadingField, WorldMap}
import com.agario.utils.{Circle, Line, Point}

import scala.collection.mutable
import scala.collection.mutable.Set

object World {

  var chartWidth = 0
  var chartHeight = 0
  var edgesLines = Array[Line]()
  var hiddenCellFactor = 0.0

  var mapCenter = Point.zero
  var tick = 0

  var config : Config = null
  val staticEntitiesTypes = scala.collection.immutable.Set(BaseEntity.food, BaseEntity.ejection)
  val worldMapEntitiesTypes = scala.collection.immutable.Set(BaseEntity.food, BaseEntity.virus, BaseEntity.ejection)

  def init (config : Config): Unit =  {
    this.config = config
    this.chartWidth = (config.width / BaseField.defaultPropose).floor.toInt
    this.chartHeight = (config.height / BaseField.defaultPropose).floor.toInt
    this.edgesLines = Array(new Line(1,0,0), new Line(0,1,0), new Line(1,0, -config.width), new Line(0,1, -config.height))
    this.hiddenCellFactor = FadingField.getStartFadingFactor(BaseField.defaultPropose, chartWidth, chartHeight)

    this.mapCenter = new Point(config.width / 2, config.height / 2)
    this.simpleFragment = new Fragment(
      "-1.1",
      new Circle(
        Point.zero,
        Fragment.radiusByWeight(Fragment.startWeight)
      ),
      Fragment.startWeight,
      Point.zero,
      0
    )
    this.staticEntitiesField = new EntitiesField(staticEntitiesTypes)

    this.lastVisiblePlayer =  new Player(
      "-1",
      new Circle(
        new Point(50, 50),
        Fragment.radiusByWeight(Player.playerStartWeight)
      ),
      Point.zero,
      Player.playerStartWeight
    )

    fragmentPrevStates = Map.empty[String, BaseEntity]
    entityPrevStates = Map.empty[String, BaseEntity]
    fragments.clear()
    entities.clear()
    players.clear()

    isWorldChanged = false

    maxRadiusFragment = None
    maxRadiusFragmentCount = 0

    worldField = None
    worldFieldUpdatedTick = -1

    entitiesHistoryTimeline.clear()
    entitiesHistory.clear()

    tick = 0
  }

  var staticEntitiesField : EntitiesField = null

  val entitiesHistoryTimeline = scala.collection.mutable.HashMap[String, Int]()
  val entitiesHistory = scala.collection.mutable.HashMap[String, BaseEntity]()

  var entityPrevStates = scala.collection.Map.empty[String, BaseEntity]
  var fragmentPrevStates = scala.collection.Map.empty[String, BaseEntity]


  val fragments = mutable.HashMap.empty[String, Fragment]
  val entities = mutable.HashMap.empty[String, BaseEntity]
  val players =  mutable.HashMap.empty[String, Player]

  var simpleFragment : Fragment = null
  var lastVisiblePlayer : Player = null

  var isWorldChanged = false

  var maxRadiusFragment : Option[Fragment] = None
  var maxRadiusFragmentCount = 0

  var worldField : Option[BaseField] = None
  var worldFieldUpdatedTick = -1
  val newEntitiesType = mutable.Set[String]()
  val newEntitiesIds = mutable.Set[String]()
  var minFragmentWeight = 1000.0


  def updateEntities(fEntities : Array[Entity],
                     eEntities : Array[Entity]): Unit = {
    val visibleCells = Set[Point]()

    fragmentPrevStates ++= fragments
    entityPrevStates ++= entities
    val curEntitiesIds = mutable.Set[String]()
    val curFragmentIds = mutable.Set[String]()

    minFragmentWeight = 1000.0

    fEntities.foreach {
      case e =>
        val fragment = if (fragments.contains(e.getId())) {
            getFragment(e.getId()).
              update(new Circle(e.point, e.r.get), e.speed, e.weight.get, e.ttf.getOrElse(0))
          } else {
            e.fragment()
          }
        minFragmentWeight = math.min(minFragmentWeight, fragment.weight)

        visibleCells ++= fragment.visibleCells//@todo do it smart
        fragment.fadeField()
        fragments.put(e.getId(), fragment)

        curFragmentIds.add(e.getId())
    }

    eEntities.foreach{
      case e =>
        val newEntity = e.getEntity()

        if (newEntity.eType != BaseEntity.fragment) {
          if (newEntity.eType == BaseEntity.food && !entitiesHistory.contains(newEntity.getId())) {
            val pos1 = new Point(
              e.point.x,
              World.config.height - e.point.y
            )
            val eFood1 = new Entity(None, pos1.x, pos1.y, e.objectType, e.weight, e.r, None, None, None, None).getEntity()

            staticEntitiesField.updateEntity(eFood1)
            visibleCells.add(staticEntitiesField.pointCell(eFood1.posCircle.point))
            entitiesHistory.put(eFood1.getId(), eFood1)
            entitiesHistoryTimeline.put(eFood1.getId(), tick)

            val pos2 = new Point(
              World.config.width - e.point.x,
              e.point.y
            )
            val eFood2 = new Entity(None, pos2.x, pos2.y, e.objectType, e.weight, e.r, None, None, None, None).getEntity()

            staticEntitiesField.updateEntity(eFood2)
            visibleCells.add(staticEntitiesField.pointCell(eFood2.posCircle.point))
            entitiesHistory.put(eFood2.getId(), eFood2)
            entitiesHistoryTimeline.put(eFood2.getId(), tick)

            val pos3 = new Point(
              World.config.width - e.point.y,
              World.config.height - e.point.y
            )

            val eFood3 = new Entity(None, pos3.x, pos3.y, e.objectType, e.weight, e.r, None, None, None, None).getEntity()

            staticEntitiesField.updateEntity(eFood3)
            visibleCells.add(staticEntitiesField.pointCell(eFood3.posCircle.point))
            entitiesHistory.put(eFood3.getId(), eFood3)
            entitiesHistoryTimeline.put(eFood3.getId(), tick)
          }

          entitiesHistory.put(newEntity.getId(), newEntity)
          staticEntitiesField.updateEntity(newEntity)
          entitiesHistoryTimeline.put(newEntity.getId(), tick)
        }

        if (entities.contains(e.getId())) {
          val entity = getEntity(e.getId()).get
          entity.update(newEntity.posCircle, newEntity.speed, newEntity.weight)

          fragments.values.foreach(f => f.updateEntity(entity))

          entities.put(e.getId(), entity)

        } else {
          isWorldChanged = true

          newEntitiesType.add(newEntity.eType)
          newEntitiesIds.add(newEntity.getId())

          fragments.values.foreach(f => f.addEntity(newEntity))
          entities.put(e.getId(), newEntity)
        }
        curEntitiesIds.add(e.getId())
    }

    entityPrevStates.filter{
      case(eId, entity) => !curEntitiesIds.contains(eId)
    }.foreach{
      case (eId, entity) =>
        fragments.values.foreach(f => f.fadeEntity(entity))
        entities.remove(eId)
    }

    fragmentPrevStates.filter{
      case(eId, entity) => !curFragmentIds.contains(eId)
    }.foreach{
      case (eId, entity) =>
        fragments.remove(eId)
    }

    players.clear()
    players ++= entities.
      filter{
        case (id, e) => e.eType == BaseEntity.player
      }.
      map{
        case (id, player)  =>
          (id, player.asInstanceOf[Player])
      }

    entitiesHistory.retain{//must be visible but not visible, then remove
      case (id,e) =>
        val isEliminated = Fragment.isVisible(fragments.values, e) && !entities.contains(e.getId())

        if (isEliminated) {
          entitiesHistoryTimeline.remove(id)
          staticEntitiesField.removeEntity(id)
        }

        !isEliminated
    }

    entitiesHistoryTimeline.retain{
      case(eId, t) =>
        if (t < (tick - EntitiesField.maxEntityHistory)) {
          staticEntitiesField.removeEntity(eId)
          entitiesHistory.remove(eId)
        }

        !(t < (tick - EntitiesField.maxEntityHistory))
    }

    staticEntitiesField.setVisibleCells(visibleCells.toSet, tick)
  }

  def updateWorld (fEntities : Array[Entity],
                   eEntities : Array[Entity],
                   _tick : Int
                    ): Unit = {
    isWorldChanged = false

    newEntitiesType.clear()
    newEntitiesIds.clear()

    tick = _tick
    updateEntities(fEntities, eEntities)

    lastVisiblePlayer = if (players.size > 0) players.values.head else lastVisiblePlayer
    fragments.values.foreach{
      case f =>
        f.fadeField()
    }
  }


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
    entities.filter{case(id, e) => e.eType == etype}.toMap
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

  def getFieldsSum(): BaseField = {
    if (worldFieldUpdatedTick == tick && worldField.isDefined) {
      worldField.get
    } else {
      worldField = Some(
        {
          if (fragments.size > 0) {
            val smalls = fragments.values.filter(t => t.weight < 80)
            val bigs = fragments.values.filter(t => t.weight >= 80)

            var field : BaseField = null

            if (smalls.size > 0) {
              field = smalls.head.getFieldsSum()
            }

            if (bigs.size > 0) {
              if (field != null)
                field.sum(bigs.head.getFieldsSum())
              else
                field = bigs.head.getFieldsSum()
            }

            World.staticEntitiesField.sum(field, tick)
          } else {
            World.staticEntitiesField
          }
        }
      )
      worldField.get
    }
  }

  def getNearestPointToEdge(point : Point): Point = {
    val minAxis = math.min(point.x, point.y)

    if (point.x + point.y <= config.width / 2) {
      if (minAxis == point.x) {
        return new Point(0, point.y)
      } else {
        return new Point(point.x, 0)
      }
    } else {
      if (minAxis == point.x) {
        return new Point(point.x, config.height)
      } else {
        return new Point(config.width, point.y)
      }
    }
  }
}
