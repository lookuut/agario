package com.agario.models

import com.agario.Config
import com.agario.utils.{Circle, Point}
import com.rojoma.json.v3.util.SimpleJsonCodecBuilder


class Entity(val id : Option[String],
             val x : Double,
             val y : Double,
             val objectType : Option[String],
             val weight : Option[Double],
             val r : Option[Double],
             val sx : Option[Double],
             val sy : Option[Double],
             val pId : Option[Int],
             val ttf : Option[Int]
            ) {
  val point = new Point(x, y)
  val speed = if (sx.isDefined) new Point(sx.get, sy.get) else Point.zero

  override def toString() = f"""$id $point $objectType $weight $r $speed $ttf"""

  def getId (): String = {
    BaseEntity.getEntityId(this)
  }

  def fragment(world: World) : Fragment = {
    new Fragment(world, id.get, new Circle(point, r.get), weight.get, speed, ttf.getOrElse(0))//@todo, what should we do with radius
  }

  def virus(world: World) : Virus = {
    new Virus(world, id.get, new Circle(point, world.config.virusRadius), Point.zero, weight.get)
  }

  def food(world: World) : Food = {
    new Food(world, point.toString, new Circle(point, Food.radius), Point.zero, world.config.foodWeight)
  }

  def player(world: World) : Player = {
    val lastState = world.entityPrevStates.get(getId())
    val lastPos = if (lastState.isEmpty) point else lastState.get.posCircle.point
    val speed = Player.speedByLastPos(point, lastPos)

    new Player(world, id.get, new Circle(point, r.get), speed, weight.get)
  }

  def ejection(world: World) : Ejection = {
    val lastState = world.entityPrevStates.get(getId())
    val lastPos = if (lastState.isEmpty) point else lastState.get.posCircle.point
    val speed = Player.speedByLastPos(point, lastPos)

    new Ejection(world, id.get, new Circle(point, Ejection.radius), speed, Ejection.weight, pId.get)
  }

  def getEntity(world: World): BaseEntity = {
    if (objectType.isEmpty) {
      fragment(world)
    } else if (objectType.get == BaseEntity.player) {
      player(world)
    } else if (objectType.get == BaseEntity.food) {
      food(world)
    } else if (objectType.get == BaseEntity.ejection) {
      ejection(world)
    } else if (objectType.get == BaseEntity.virus) {
      virus(world)
    } else {
      throw new Exception("Unknown type " + objectType.get)
    }
  }

  def isStatic(): Boolean = {
    if (objectType.isDefined && (objectType.get == "F" || objectType.get == "V")) {
      true
    } else {
      false
    }
  }
}

object Entity {
  implicit val jCodec = SimpleJsonCodecBuilder[Entity].
   build("Id", _.id,
    "X", _.x,
    "Y", _.y,
    "T", _.objectType,
    "M", _.weight,
    "R", _.r,
    "SX", _.sx,
    "SY", _.sy,
     "pId", _.pId,
    "TTF", _.ttf
  )
}


