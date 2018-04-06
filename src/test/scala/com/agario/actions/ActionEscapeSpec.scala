package com.agario.actions

import com.agario.TestWorld
import com.agario.models.{Fragment, Player}
import com.agario.utils.{Circle, Point}

class ActionEscapeSpec extends org.specs2.mutable.Specification {
  "Escape test" should {

    "Test isDangerAround" in {

      val fragments = Map("1" -> new Fragment("1", new Circle(new Point(100, 100), 40 / math.sqrt(40)), 40.0, new Point(0, 0), None))
      val players = Map("1" -> new Player("1", new Circle(new Point(10, 10), 2.5), 40.0, new Point(3, 3)))

      val world = new TestWorld().getWorld()
      world.updateFragments(fragments)
      world.updatePlayers(players)

      ActionEscape.isDangerAround(world) mustEqual None

      val enemy = new Player("1", new Circle(new Point(10, 10), 2.5), 60.0, new Point(3, 3))
      world.updatePlayers(Map("1" -> enemy))
      ActionEscape.isDangerAround(world) mustEqual Some(new ActionEscape(enemy.id, world))
    }

    "Test escape with split" in {

      val world = new TestWorld().getWorld()

      val enemy = new Player("1", new Circle(new Point(10, 10), 2.5), 60.0, new Point(3, 3))
      val enemyComeTick = Fragment.positionTick(enemy, new Point(100,100), world.config)._1

      val fragments = Map(
        "1" -> new Fragment("1", new Circle(new Point(100, 100), 40 / math.sqrt(40)), 40.0, new Point(0, 0), Some(enemyComeTick - 10)),
        "2" -> new Fragment("2", new Circle(new Point(120, 120), 40 / math.sqrt(40)), 40.0, new Point(0, 0), Some(enemyComeTick - 10))
      )

      world.updateFragments(fragments)
      world.updatePlayers(Map("1" -> enemy))

      var escapeResult = ActionEscape.isDangerAround(world)
      escapeResult.get.isInstanceOf[ActionUnite] mustEqual true

      world.updateFragments(Map(
        "1" -> new Fragment("1", new Circle(new Point(100, 100), 40 / math.sqrt(40)), 40.0, new Point(0, 0), Some(enemyComeTick + 10)),
        "2" -> new Fragment("2", new Circle(new Point(120, 120), 40 / math.sqrt(40)), 40.0, new Point(0, 0), Some(enemyComeTick + 10))
      ))

      escapeResult = ActionEscape.isDangerAround(world)
      escapeResult.get.isInstanceOf[ActionEscape] mustEqual true

      world.updateFragments(Map(
        "1" -> new Fragment("1", new Circle(new Point(100, 100), 20 / math.sqrt(40)), 20.0, new Point(0, 0), Some(enemyComeTick - 10)),
        "2" -> new Fragment("2", new Circle(new Point(120, 120), 20 / math.sqrt(40)), 20.0, new Point(0, 0), Some(enemyComeTick - 10))
      ))

      escapeResult = ActionEscape.isDangerAround(world)
      escapeResult.get.isInstanceOf[ActionEscape] mustEqual true
    }
  }
}

