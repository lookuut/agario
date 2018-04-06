package com.agario

import com.agario.models._
import com.agario.utils.{Circle, Point}

class TestWorld {

  private val config = new Config(990,
    990,
    5000,
    10,
    12,
    300,
    10,
    120,
    1,
    2,//inertion factor
    50)//speed factor

  private val staticFragments = Map("1" -> new Fragment("1", new Circle(new Point(100, 100), 40 / math.sqrt(40)), 40.0, new Point(0,0), None))
  private val movingFragments = Map("1" -> new Fragment("1", new Circle(new Point(100, 100), 40 / math.sqrt(40)), 40.0, new Point(3,0), None))

  private val viruses = Map.empty[String, Virus]
  private val foods = Map(
    new Point(90, 100) -> new Food(new Circle(new Point(90, 100), Config.foodRadius), 2),
    new Point(112, 100) -> new Food(new Circle(new Point(112, 100),  Config.foodRadius), 2),
    new Point(50, 50) -> new Food(new Circle(new Point(50, 50),  Config.foodRadius), 2),
    new Point(Config.foodRadius, Config.foodRadius) -> new Food(new Circle(new Point(Config.foodRadius, Config.foodRadius),  Config.foodRadius), 2)
  )

  private val ejections = Array.empty[Ejection]
  private val players = Map("1.1" -> new Player("1.1", new Circle(new Point(30, 30), 40 / math.sqrt(40)), 40, new Point(0,0)))


  def getWorld(): World = {
    val world = new World(config)
    world.updateWorld(staticFragments, viruses, foods, ejections, players, 0)
    world

  }

  def getWorldWithMovingFragment(): World = {
    val world = new World(config)
    world.updateWorld(movingFragments, viruses, foods, ejections, players, 0)
    world
  }
}
