package com.agario.actions

import com.agario.TestWorld
import com.agario.commands.{Move, Split}
import com.agario.models.{Fragment, Player}
import com.agario.utils.{Circle, Point}

class ActionCatchSpec  extends org.specs2.mutable.Specification {
  "Action catch victims test" should {

    "Test find victims" in {

      val fragments = Map("1" -> new Fragment("1", new Circle(new Point(100, 100), 40 / math.sqrt(40)), 40.0, new Point(0, 0), None))
      val players = Map("1" -> new Player("1", new Circle(new Point(10, 10), 2.5), 40.0, new Point(3, 3)))

      val world = new TestWorld().getWorld()
      world.updateFragments(fragments)
      world.updatePlayers(players)

      ActionCatch.searchVictims(world) mustEqual None

      val enemy1 = new Player("1", new Circle(new Point(10, 10), 2.5), 20.0, new Point(3, 3))
      val enemy2 = new Player("2", new Circle(new Point(200, 200), 2.5), 20.0, new Point(3, 3))

      world.updatePlayers(Map("1" -> enemy1, "2" -> enemy2))

      ActionCatch.searchVictims(world).get mustEqual enemy1
    }

    "Test find victims at moving" in {
      val fragments = Map("1" -> new Fragment("1", new Circle(new Point(100, 100), 40 / math.sqrt(40)), 40.0, new Point(-3, 0), None))

      val enemy1 = new Player("1", new Circle(new Point(0, 100), 2.5), 20.0, new Point(3, 3))
      val enemy2 = new Player("2", new Circle(new Point(200, 100), 2.5), 20.0, new Point(3, 3))


      val world = new TestWorld().getWorld()

      world.updateFragments(fragments)
      world.updatePlayers(Map("1" -> enemy1, "2" -> enemy2))

      ActionCatch.searchVictims(world).get mustEqual enemy1
    }

    "Test catch" in {
      val fragments = Map("1" -> new Fragment("1", new Circle(new Point(100, 100), 40 / math.sqrt(40)), 120.0, new Point(-3, 0), None))

      val enemy1 = new Player("1", new Circle(new Point(0, 100), 2.5), 20.0, new Point(3, 3))
      val enemy2 = new Player("2", new Circle(new Point(200, 100), 2.5), 20.0, new Point(3, 3))

      val world = new TestWorld().getWorld()

      world.updateFragments(fragments)
      world.updatePlayers(Map("1" -> enemy1, "2" -> enemy2))

      val catch1 = new ActionCatch("1", world)

      catch1.run().isInstanceOf[Split] mustEqual true

      val catch2 = new ActionCatch("2", world)
      catch2.run().isInstanceOf[Move] mustEqual true
    }
  }
}

