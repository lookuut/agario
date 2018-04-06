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
  val speed = if (sx.isDefined) Some(new Point(sx.get, sy.get)) else None

  override def toString() = f"""$id $point $objectType $weight $r $speed $ttf"""

  def fragment(world: World) : Fragment = {
    new Fragment(id.get, new Circle(new Point(x, y), Fragment.radiusByWeight(weight.get)), weight.get, speed.get, ttf)
  }

  def virus(world: World) : Virus = {
    new Virus(id.get, new Circle(new Point(x, y), world.config.virusRadius), weight.get)
  }

  def food(world: World) : Food = {
    new Food(new Circle(new Point(x, y), Config.foodRadius), world.config.foodWeight)
  }

  def player(playerLastPos : Option[Point]) : Player = {
    val pos = new Point(x, y)
    val speed = Player.speedByLastPos(pos, playerLastPos.getOrElse(pos))

    new Player(id.get, new Circle(new Point(x, y), r.get), weight.get, speed)
  }

  def ejection() : Ejection = {
    new Ejection(pId.get, new Point(x, y))
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


