package com.agario.models

import com.agario.Config
import com.agario.navigation.{FadingField, FragmentField, Track}
import com.agario.utils.{Circle, Line, Point}

import scala.collection.mutable.HashMap

class Fragment(world: World,
                id : String,
               var posCircle : Circle,
               var weight : Double,
               var speed : Point,
               var ttf : Int
               ) extends BaseEntity(id, eType = BaseEntity.fragment, world) {

  val mapEntities = HashMap.empty[String, BaseEntity]//all entities in map, if entity not visible then save last state
  val entityAtFragmentCells = HashMap.empty[String, Point]
  val entityLastFactor = HashMap.empty[String, Map[Point, Double]]
  val tracks = HashMap.empty[Point, Track]

  val field = new FragmentField(world)
  val fadingField = new FadingField(world, 0)

  val playerId = if (id.contains('.')) id.split('.').head.toInt else id.toInt
  var fragmentCell = field.pointCell(posCircle.point)
  var visionRadius = Fragment.visionRadius(this, world.fragments.size)

  def maxSpeed(config : Config): Double = {
    config.speedFactor / math.sqrt(weight)
  }

  def update(posCircle: Circle, speed: Point, weight: Double, ttf : Int): Fragment = {
    super.update(posCircle, speed, weight)
    this.ttf = ttf
    visionRadius = Fragment.visionRadius(this, world.fragments.size)
    this
  }

  def getVectorFactor(startPoint : Point, direction : Point): (Double, Iterable[Point]) = {
    val (factor1 , cells1) = field.vectorFactor(startPoint, direction)
    val (factor2 , cells2) = fadingField.vectorFactor(startPoint, direction)

    ((factor1 + factor2) / cells1.size, cells1)
  }

  def getFragmentRelativeCell (point : Point): Point = {
    (field.pointCell(point) - fragmentCell)
  }

  def updateMapEntities(): Unit = {
    val fragmentCurCell = field.pointCell(posCircle.point)
    if (fragmentCurCell != fragmentCell) {
      tracks.clear()
      fragmentCell = fragmentCurCell
    }

    val fadingEntities = mapEntities.filter(e => e._2.eType != BaseEntity.virus).filter {//entity was disappear add to fading map
      case (eId, entity) =>
        (
          (
              !world.entities.contains(eId)
          )
        )
    }

    fadingEntities.foreach {
      case(id, entity) =>
        fadeEntity(entity)
    }
    mapEntities ++= world.entities.filter{case(id, e) => !mapEntities.contains(id)}
    mapEntities.values.filter(e => e.eType != BaseEntity.fragment).foreach{
      case e =>
        val entityAtFragmentCell = getFragmentRelativeCell(e.posCircle.point)

        if (!entityAtFragmentCells.contains(e.getId())) {//new entity
          val trackFactors = e.factor(this)

          field.apply(trackFactors._1)
          entityLastFactor.put(e.getId(), trackFactors._1)

          entityAtFragmentCells.put(e.getId(), entityAtFragmentCell)
          tracks.put(entityAtFragmentCell, trackFactors._2)

        } else if (entityAtFragmentCells.get(e.getId()).get != entityAtFragmentCell) {//entity change position
          val trackFactors = e.factor(this)

          field.apply(trackFactors._1)//set new
          field.apply(entityLastFactor.get(e.getId()).get, -1)//remove old

          tracks.put(entityAtFragmentCell, trackFactors._2)//set new
          tracks.remove(entityAtFragmentCells.get(e.getId()).get)//remove old

          entityLastFactor.put(e.getId(), trackFactors._1)
          entityAtFragmentCells.put(e.getId(), entityAtFragmentCell)
        }
    }

    world.fragments.values.
      filter(e => e.eType == BaseEntity.fragment).
      foreach{
        case e =>
          val entityCell = field.pointCell(e.posCircle.point)

          if (!entityAtFragmentCells.contains(e.getId())) {//new entity
            val factors = e.factor(this)._1
            fadingField.apply(factors)
            entityLastFactor.put(e.getId(), factors)
            entityAtFragmentCells.put(e.getId(), entityCell)
          } else if (entityAtFragmentCells.get(e.getId()).get != entityCell) {//entity change position
            val factors = e.factor(this)._1
            fadingField.apply(factors)
            entityLastFactor.put(e.getId(), factors)
            entityAtFragmentCells.put(e.getId(), entityCell)
          }
      }
  }

  def fadeField(): Unit = {
    fadingField.cellsPowerFading(Set.empty[Point])
  }

  def fadeEntity(entity: BaseEntity) : Unit = {
    field.set(entityLastFactor.get(entity.getId()).get, 0)
    fadingField.apply(entity.fadingFactor(entityLastFactor.get(entity.getId()).get))
    mapEntities.remove(entity.getId())
    entityAtFragmentCells.remove(entity.getId())
    entityLastFactor.remove(entity.getId())
  }

  def getVisibleFactorField (): Map[Point, Double] = {
    field.getCircleCells(visionCenter(), visionRadius).map {
        case p =>
          val direction = getFragmentRelativeCell(new Point(p.x * field.propose, p.y * field.propose))
          val factor = (
                        field.cellFactor(new Point(p.x, p.y))
                        +
                        fadingField.cellFactor(new Point(p.x, p.y))
            )
          (direction, factor)
      }.toMap
  }

  override def isStatic(): Boolean = false
  override def toString() = f"""$id $posCircle $weight $speed $ttf"""

  override def factor(fragment: Fragment): (Map[Point, Double], Track) = {
    (Map.empty[Point, Double], Track.empty)
  }

  override def fadingFactor(lastFactor: Map[Point, Double]): Map[Point, Double] = {
    lastFactor
  }

  def getFieldsSum(): FragmentField = {
    field.sum(fadingField)
  }

  def visionCenter() : Point = {
    posCircle.point + speed.normalize() * Fragment.movingFragmentVisionFactor
  }

  def canBurstOnVirus(): Boolean = {

    (
      weight >= Virus.burstMinWeight
        &&
      posCircle.r >= world.config.virusRadius
        &&
      world.fragments.size < world.config.maxFragmentsCount
    )
  }
}

object Fragment {

  val factor = 1.0f
  val movingFragmentVisionFactor = 10

  def isVisibleNextTick (fragments : Iterable[Fragment], entity : BaseEntity) : Boolean = {
    fragments.filter {
      case fragment =>
        val fVisionRadius = visionRadius(fragment, fragments.size)
        val visionCenter = fragment.speed.normalize() * movingFragmentVisionFactor + fragment.posCircle.point

        visionCenter.distance(entity.posCircle.point + entity.speed) < fVisionRadius + entity.posCircle.r
    }.size > 0
  }

  def isVisible(fragments : Iterable[Fragment], entity : BaseEntity) : Boolean = {

    fragments.filter {
      case fragment =>
        val fVisionRadius = visionRadius(fragment, fragments.size)
        val visionCenter = fragment.speed.normalize() * 10 + fragment.posCircle.point

        visionCenter.distance(entity.posCircle.point) < fVisionRadius + entity.posCircle.r
    }.size > 0
  }

  def maxVisionRadius (fragments : Iterable[Fragment]): Double = {
    fragments.map(f => visionRadius(f, fragments.size)).max
  }

  def visionRadius(fragment: BaseEntity, fragmentCount : Int): Double = {
    if (fragmentCount >= 2) {
      fragment.posCircle.r * 2.5 * math.sqrt(fragmentCount)
    } else {
      fragment.posCircle.r * 4
    }
  }

  def positionOnTick(fragment : BaseEntity, tick : Int, direction : Point, config : Config) : Point = {
    val maxSpeed = config.speedFactor / math.sqrt(fragment.weight)
    positionOnTick(tick, fragment.posCircle.point, direction.normalize(), fragment.weight, fragment.speed, maxSpeed, config)
  }

  def positionTick (fragment: BaseEntity, pos : Point, config: Config): (Int, Map[Int, Point]) = {
    val track = HashMap.empty[Int, Point]
    val tick = positionTick(0, pos, fragment.posCircle.point, fragment, fragment.speed, config.speedFactor / math.sqrt(fragment.weight), config, false, track)

    (tick, track.toMap)
  }

  def moveWithCorrectionTick (fragment: BaseEntity, pos : Point, config: Config): (Int, Map[Int, Point]) = {
    val track = scala.collection.mutable.HashMap.empty[Int, Point]
    val tick = positionTick(0, pos, fragment.posCircle.point, fragment, fragment.speed, config.speedFactor / math.sqrt(fragment.weight), config, true, track)
    (tick, track.toMap)
  }

  def getCorrectionDirect(speed : Point, targetVec : Point, maxSpeed : Double): Point = {

    val angle = speed.angle(targetVec)
    if (speed.length() >= maxSpeed * 0.7 && angle > math.Pi / 20 && angle < math.Pi / 2) {
      //if (angle <= math.Pi / 4) {
        speed.mirrorVector(targetVec).normalize()
      //} else {
        //targetVec.turn(-math.Pi / 4).normalize()
      //}
    } else
      targetVec.normalize()
  }

  def positionTick(tick : Int,
                   targetPos : Point,
                   curPos : Point,
                   fragment: BaseEntity,
                   speed : Point,
                   maxSpeed : Double,
                   config : Config,
                   moveCorrection : Boolean = false,
                   track : HashMap[Int, Point]
                  ) : Int = {

    val targetVec = (targetPos - curPos)

    val direction = {
      if (moveCorrection) {
        getCorrectionDirect(speed, targetVec, maxSpeed)
      } else
        targetVec.normalize()
    }

    val newSpeed = tickSpeed(direction, maxSpeed, speed, config.inertionFactor / fragment.weight)
    val newPos = curPos + newSpeed
    track += (tick + 1 -> newPos)

    if ((targetPos - newPos).length() <= fragment.posCircle.r) {//@TODO fix high speed
      tick + 1
    } else {
      positionTick(tick + 1, targetPos, newPos, fragment, newSpeed, maxSpeed, config, moveCorrection, track)
    }
  }

  def positionOnTick(tick : Int, point : Point, direction : Point, weight : Double, speed : Point, maxSpeed : Double, config : Config) : Point = {

    val newSpeed = tickSpeed(direction, maxSpeed, speed, config.inertionFactor / weight)
    val newPos = point + newSpeed

    if (tick - 1 > 0) {
      positionOnTick(tick - 1, newPos, direction, weight, newSpeed, maxSpeed, config)
    } else {
      newPos
    }
  }

  def tickSpeed (direction : Point, maxSpeed : Double, speed : Point, factor : Double): Point = {
    val x = (direction.x * maxSpeed - speed.x) * factor + speed.x
    val y = (direction.y * maxSpeed - speed.y) * factor + speed.y
    new Point(x, y)
  }

  def maxSpeed(weight : Double, config : Config): Double = {
    config.speedFactor / math.sqrt(weight)
  }

  def radiusByWeight(weight : Double): Double = {
    2 * math.sqrt(weight)
  }
}
