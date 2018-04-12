package com.agario

import com.agario.models._
import com.agario.utils.Point

class BaseSpec extends org.specs2.mutable.Specification {

  private val config = new Config(
    990,
    990,
    75000,
    1f,
    12,
    300,
    22f,//virus radius
    120,
    0.25f,
    1f,//inertion factor
    35f)//speed factor


  val fragmentWeight = 40
  val fragmentMaxSpeed = Fragment.maxSpeed(fragmentWeight, config)

  def getWorld(): World = {
    val world = new World(config)
    world
  }

  def getEFragment(pos : Point, speed : Point, weight: Double = fragmentWeight) : Entity = {
    new Entity(Some("1"), pos.x, pos.y, None, Some(weight), Some(Fragment.radiusByWeight(weight)), Some(speed.x), Some(speed.y), None, None)
  }

  def getEFood(pos : Point): Entity = {
    new Entity(Some("1"), pos.x, pos.y, Some(BaseEntity.food), Some(config.foodWeight), Some(Food.radius), Some(0), Some(0), None, None)
  }

  def getEPlayer(pos : Point, weight: Double = fragmentWeight): Entity = {
    new Entity(Some("1"), pos.x, pos.y, Some(BaseEntity.player), Some(weight), Some(weight / math.sqrt(40)), None, None, None, None)
  }

  def getEVirus(pos : Point): Entity = {
    new Entity(Some("1"), pos.x, pos.y, Some(BaseEntity.virus), Some(Virus.weight), Some(config.virusRadius), Some(0), Some(0), None, None)
  }

  def getEEjection(pos : Point): Entity = {
    new Entity(Some("1"), pos.x, pos.y, Some(BaseEntity.ejection), Some(Ejection.weight), Some(Ejection.radius), None, None, Some(2), None)
  }

  def getFragment (pos : Point, speed : Point, weight: Double = fragmentWeight) : Fragment = {
    getEFragment(pos, speed, weight).fragment(getWorld())
  }

}
