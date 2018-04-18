package com.agario.models

import com.agario.navigation.{BaseField, Track}
import com.agario.utils.{Circle, Point, Trajectory}

class Player (
               id : String,
              var posCircle : Circle,
              var speed : Point,
              var weight : Double
            ) extends BaseEntity(id, eType = BaseEntity.player)
{

  var inertion : Double = BaseEntity.inertion(weight)
  var maxSpeed : Double = BaseEntity.maxSpeed(weight)

  val playerId = if (id.contains('.')) id.split('.').head.toInt else id.toInt
  val isSplited = id.contains('.')
  val visionRadius = Fragment.visionRadius(this, if (isSplited) 2 else 1)//@todo : get real player's fragments

  var fragmentCell = World.staticEntitiesField.pointCell(posCircle.point)
  var visibleCells = World.staticEntitiesField.getCircleCells(visionCenter(), visionRadius)

  override def update(posCircle: Circle, speed: Point, weight: Double): BaseEntity = {
    super.update(posCircle, speed, weight)

    if (fragmentCell != World.staticEntitiesField.pointCell(posCircle.point)) {//fragment change pos, update visible cells
      visibleCells = World.staticEntitiesField.getCircleCells(visionCenter(), visionRadius)
      fragmentCell = World.staticEntitiesField.pointCell(posCircle.point)
    }
    this
  }
  override def equals(that: Any): Boolean =
    that match {
      case that: Player => that.id == id
      case _ => false
    }

  override def hashCode() : Int = {
    id.hashCode
  }

  override def canEat(fragment: Fragment): Boolean = {
    Player.factor(fragment, (World.fragments.size >= 2), this, isSplited) < 0
  }

  override def copy(): BaseEntity = {
    new Player(id, posCircle, speed, weight)
  }


  override def factor(fragment: Fragment): Map[Point, Double] = {

    val factor = Player.factor(fragment, (World.fragments.size >= 2), this, isSplited)

    World.staticEntitiesField.setVisibleCells(fragment.
      field.
      getCircleCells(
        this.posCircle.point,
        this.visionRadius
      ))

    if (factor > 0) {
      val visionRadius = Fragment.visionRadius(this, 1) + Player.playerVisionRadiusDelta//@todo do it well , with player fragments count
      val visionCells = fragment.field.getCircleCells(posCircle.point, visionRadius)

      Trajectory.playerDangerField(this, Food.coverPart, visionCells, factor)
    } else if (factor < 0) {
      val visionCells = fragment.field.getCircleCells(posCircle.point, fragment.visionRadius)
      fragment.field.interpolation(posCircle.point, Point.zero, factor, visionCells)
    } else {
      Map.empty[Point, Double]
    }
  }
  override def factorValue(fragment : Fragment) : Double = {
    Player.factor(fragment, (World.fragments.size >= 2), this, isSplited)
  }


  override def fadingFactor(lastFactors : Map[Point, Double]): Map[Point, Double] = {
    lastFactors
  }

  override def isStatic(): Boolean = false

  def visionCenter() : Point = {
    posCircle.point + speed.normalize() * Fragment.movingFragmentVisionFactor
  }
}


object Player {
  val playerStartWeight = 50f
  val playerVisionRadiusDelta = 20
  val coverPart = 2.0f / 3
  val predatorFactor = 1.2f

  val fragmentFactor = 10
  val playerFactor = 100

  def speedByLastPos(currentPos : Point, lastPos : Point) : Point = {
     (currentPos - lastPos)
  }


  def factor (entity1: BaseEntity, entitySplited1 : Boolean, entity2: BaseEntity, entitySplited2 : Boolean): Double = {
    val wk = entity1.weight / entity2.weight

    if (wk >= 1f/predatorFactor && wk <= predatorFactor) {
      return 0f
    } else if (wk < 1f / predatorFactor) {
      if (entitySplited1) {
        fragmentFactor
      } else {
        playerFactor
      }
    } else {
      if (entitySplited2) {
        -fragmentFactor
      } else {
        -playerFactor
      }
    }
  }
}