package com.agario.models

import com.agario.Config
import com.agario.navigation.{BaseField, FadingField, Step, Track}
import com.agario.utils.{Circle, Line, Point, Trajectory}

import scala.collection.mutable.HashMap

class Fragment(id : String,
               var posCircle : Circle,
               var weight : Double,
               var speed : Point,
               var ttf : Int
               ) extends BaseEntity(id, eType = BaseEntity.fragment) {

  var inertion : Double = BaseEntity.inertion(weight)
  var maxSpeed : Double = BaseEntity.maxSpeed(weight)

  val mapEntities = HashMap.empty[String, BaseEntity]//all entities in map, if entity not visible then save last state

  val entityAtFragmentCells = HashMap.empty[String, Point]
  val entityLastFactor = HashMap.empty[String, Map[Point, Double]]

  val field = new BaseField()
  val fadingField = new FadingField()

  val playerId = if (id.contains('.')) id.split('.').head.toInt else id.toInt
  var visionRadius = Fragment.visionRadius(this, World.fragments.size)

  val entityTypes = Set(BaseEntity.player, BaseEntity.virus)

  var fragmentCell = field.pointCell(posCircle.point)
  var visibleCells = field.getCircleCells(visionCenter(), visionRadius)
  var isChangedState = true

  var maxFactorPlayerId = ""
  var playerMaxFactor = 0.0
  var playerMaxFactorCells = scala.collection.Map.empty[Point, Double]
  var playerMaxFactorCell = Point.zero
  var edgeCell = Point.zero

  def destination(track : Track): (Point, Point) = {//position and speed
    if (track.duration() == 0) {
      return (posCircle.point, speed)
    }

    val reduceDestination : (((Int, Step), Point, Point), ((Int, Step), Point, Point)) => ((Int, Step), Point, Point) = (l, r) => {
      val newSpeed = Trajectory.tickSpeed(l._1._2.direction, l._2, maxSpeed, inertion)
      val nPos = l._3 + newSpeed

      val newPos = new Point(
        math.min(
          World.config.width - posCircle.r,
          math.max(
            posCircle.r,
            nPos.x
          )
        ),
        math.min(
          World.config.height - posCircle.r,
          math.max(
            posCircle.r,
            nPos.y
          )
        )
      )

      (r._1, newSpeed, newPos)
    }

    val sortedSteps = track.track.toSeq.sortBy(_._1)
    val destination = sortedSteps.map{
      case s =>
        (s, speed, posCircle.point)
    }.reduce(reduceDestination)

    val endSpeed = Trajectory.tickSpeed(sortedSteps.last._2.direction, destination._2, maxSpeed, inertion)
    (destination._3 + endSpeed, endSpeed)
  }

  override def copy(): BaseEntity = {
    new Fragment(id, posCircle, weight, speed, ttf)
  }

  def update(posCircle: Circle, speed: Point, weight: Double, ttf : Int): Fragment = {
    isChangedState = false

    super.update(posCircle, speed, weight)
    this.ttf = ttf

    val newVisionRadius = Fragment.visionRadius(this, World.fragments.size)
    if (newVisionRadius != visionRadius || fragmentCell != field.pointCell(posCircle.point)) {
      isChangedState = true

      visionRadius = Fragment.visionRadius(this, World.fragments.size)
      visibleCells = field.getCircleCells(visionCenter(), visionRadius)
      if (playerMaxFactor > 0) {
        applyMaxFactorCell(playerMaxFactorCells, playerMaxFactorCell, field, fragmentCell, -1)
        applyMaxFactorCell(playerMaxFactorCells, playerMaxFactorCell, field, field.pointCell(posCircle.point), 1)
      }

      fragmentCell = field.pointCell(posCircle.point)
    }

    this
  }

  override def factorValue(fragment : Fragment) : Double = {
    Double.PositiveInfinity
  }

  override def canEat(fragment: Fragment): Boolean = false

  def addEntity(e: BaseEntity): Unit = {
    if (!entityTypes.contains(e.eType)) {
      return
    }

    val entityAtFragmentCell = getFragmentRelativeCell(e.posCircle.point)
    val playerFactorCells = e.factor(this)

    if (playerFactorCells.size > 0) {
      val trackMaxFactor = playerFactorCells.maxBy(_._2)
      if (playerMaxFactor < trackMaxFactor._2) {

        applyMaxFactorCell(playerFactorCells, trackMaxFactor._1, field, fragmentCell, 1)
        applyMaxFactorCell(playerMaxFactorCells, playerMaxFactorCell, field, fragmentCell, -1)

        maxFactorPlayerId = e.getId()
        playerMaxFactor = trackMaxFactor._2
        playerMaxFactorCells = playerFactorCells
        playerMaxFactorCell = trackMaxFactor._1
      }
    }


    field.apply(playerFactorCells)
    mapEntities.put(e.getId(), e)
    entityLastFactor.put(e.getId(), playerFactorCells)
    entityAtFragmentCells.put(e.getId(), entityAtFragmentCell)
  }

  def updateEntity(e: BaseEntity): Unit = {
    if (!mapEntities.contains(e.getId())) {
      return;
    }

    val entityAtFragmentCell = getFragmentRelativeCell(e.posCircle.point)

    if (entityAtFragmentCells.get(e.getId()).get != entityAtFragmentCell) {
      val playerFactorCells = e.factor(this)


      if (playerFactorCells.size > 0) {
        val trackMaxFactor = playerFactorCells.maxBy(_._2)

        if (playerMaxFactor < trackMaxFactor._2) {
          applyMaxFactorCell(playerFactorCells, trackMaxFactor._1, field, fragmentCell, 1)
          applyMaxFactorCell(playerMaxFactorCells, playerMaxFactorCell, field, fragmentCell, -1)

          maxFactorPlayerId = e.getId()
          playerMaxFactor = trackMaxFactor._2
          playerMaxFactorCells = playerFactorCells
          playerMaxFactorCell = trackMaxFactor._1
        }
      }

      entityLastFactor.put(e.getId(), playerFactorCells)
      entityAtFragmentCells.put(e.getId(), entityAtFragmentCell)
    }
  }

  def applyMaxFactorCell(
                          maxFactorCells : Map[Point, Double],
                          maxFactorCell : Point,
                          field : BaseField,
                          fragmentCell : Point,
                          sign : Int
                        ) : Unit = {

    if (maxFactorCells.size == 0) {
      return
    }

    val direction = fragmentCell - maxFactorCell
    val angle = new Point(1, 0).angleAgainstClockWay(direction)

    val sector = {
      if (angle < math.Pi / 4 && angle >= -math.Pi / 4) {
        new Point(1, 0)
      } else if (angle < 3 * math.Pi / 4 && angle >= math.Pi / 4) {
        new Point(0, 1)
      } else if (angle <= -3 * math.Pi / 4 || angle > 3 * math.Pi / 4) {
        new Point(-1, 0)
      } else {
        new Point(0, -1)
      }
    }

    val transformVector = field.pointCell(World.getNearestPointToEdge(field.cellToPoint(fragmentCell))) - maxFactorCell +
      (if (direction.x > 0) new Point(1, 0) else Point.zero) + (if (direction.y > 0) new Point(0, 1) else Point.zero)

    val mirroredCells =
      BaseField.transform(
        BaseField.mirrorCells(
          maxFactorCells,
          maxFactorCell,
          sector
        ),
        transformVector
      ).
      filter{
        case (p, f) => !maxFactorCells.contains(p)
      }

    field.apply(mirroredCells, sign)
  }

  def updateMaxFactorPoint(maxFactor : Double, maxFactorPointCenter : Point, maxFactorCells : Set[Point]): Unit = {
    if (playerMaxFactor > maxFactor) {
      return
    }

    val playerMaxFactorPoint = playerMaxFactorCell
    val edgePoint = World.getNearestPointToEdge(posCircle.point)
    val transformVector = field.pointCell(edgePoint - playerMaxFactorPoint)

    val factors =
      BaseField.transform(
        playerMaxFactorCells
        ,
        transformVector
      )
    field.apply(factors)
  }


  def fadeEntity(entity: BaseEntity) : Unit = {
    if (!mapEntities.contains(entity.getId())) {
      return
    }

    if (playerMaxFactor > 0 && maxFactorPlayerId == entity.getId()) {

      applyMaxFactorCell(playerMaxFactorCells, playerMaxFactorCell, fadingField, fragmentCell, 1)
      applyMaxFactorCell(playerMaxFactorCells, playerMaxFactorCell, field, fragmentCell, -1)

      playerMaxFactor = 0//do it well
    }

    field.apply(entityLastFactor.get(entity.getId()).get, -1)

    if (!Fragment.isVisible(World.fragments.values, entity)) {
      fadingField.apply(entity.fadingFactor(entityLastFactor.get(entity.getId()).get))
    }

    mapEntities.remove(entity.getId())
    entityAtFragmentCells.remove(entity.getId())
    entityLastFactor.remove(entity.getId())
  }

  def getVectorFactor(startPoint : Point, direction : Point): (Double, Iterable[Point]) = {
    val field = getFieldsSum()
    val (factor , cells) = field.vectorFactor(startPoint, direction)

    ((factor) / cells.size, cells)
  }

  def getFragmentRelativeCell (point : Point): Point = {
    (field.pointCell(point) - fragmentCell)
  }

  def getMaxFactorCell(cells : Set[Point]) : (Double, Point) = {
    getFieldsSum().getMaxFactor(cells)
  }

  def fadeField(): Unit = {
    fadingField.setVisibleCells(visibleCells)
    fadingField.cellsPowerFading(Set.empty[Point])
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

  override def factor(fragment: Fragment): Map[Point, Double] = {
    Map.empty[Point, Double]
  }

  override def fadingFactor(lastFactor: Map[Point, Double]): Map[Point, Double] = {
    lastFactor
  }

  var fieldSum : Option[BaseField] = None
  var fieldSumTick = -1

  def getFieldsSum(): BaseField = {
    if (fieldSumTick == World.tick && fieldSum.isDefined) {
      fieldSum.get
    } else {
      fieldSum = Some(field.sum(fadingField))
      fieldSum.get
    }
  }

  def visionCenter() : Point = {
    posCircle.point + speed.normalize() * Fragment.movingFragmentVisionFactor
  }

  def canBurstOnVirus(): Boolean = {

    (
      weight >= Virus.burstMinWeight
        &&
      posCircle.r >= World.config.virusRadius
        &&
      World.fragments.size < World.config.maxFragmentsCount
    )
  }
}

object Fragment {

  val factor = 1.0f
  val movingFragmentVisionFactor = 10
  val startWeight = 40f

  val shrinkMinWeight = 100
  val shrinkMod = 50
  val minWeightToSplit = 120f

  def shrink(weight : Double): Double = {
    if (weight < shrinkMinWeight) {
      0
    } else {
      (weight - shrinkMinWeight) * 0.01
    }
  }

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
