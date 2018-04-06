package com.agario.actions

import com.agario.{Config, TestWorld}
import com.agario.models.{Food, Fragment}
import com.agario.utils.{Circle, Point}

class ActionEatFoodSpec  extends org.specs2.mutable.Specification {
  "Eat food test" should {

    "Search visible food in static" in {

      val world = new TestWorld().getWorld()

      val action = new ActionEatFood(world)
      val command = action.run()

      command.point mustEqual new Point(90, 100)
    }

    "Search visible food at moving" in {

      val world = new TestWorld().getWorldWithMovingFragment()

      val action = new ActionEatFood(world)
      val command = action.run()

      command.point mustEqual new Point(112, 100)
    }

    "Test food at angles" in {
      val fragments = Map("1" -> new Fragment("1", new Circle(new Point(10, 10), 40 / math.sqrt(40)), 40.0, new Point(-3,-3), None))

      val world = new TestWorld().getWorldWithMovingFragment()
      world.updateFragments(fragments)

      val action = new ActionEatFood(world)
      val command = action.run()

      command.point mustEqual new Point(50, 50)
    }

    "Test food at angles" in {

      val angleFoodPoint = new Point(Config.foodRadius + 2, Config.foodRadius + 2)

      val fragments = Map("1" -> new Fragment("1", new Circle(new Point(10, 10), 40 / math.sqrt(40)), 40.0, new Point(-3,-3), None))
      val foods = Map(
        new Point(90, 100) -> new Food(new Circle(new Point(90, 100), Config.foodRadius), 2),
        new Point(112, 100) -> new Food(new Circle(new Point(112, 100),  Config.foodRadius), 2),
        new Point(50, 50) -> new Food(new Circle(new Point(50, 50),  Config.foodRadius), 2),
        angleFoodPoint -> new Food(new Circle(angleFoodPoint,  Config.foodRadius), 2)
      )
      val world = new TestWorld().getWorldWithMovingFragment()
      world.updateFragments(fragments)
      world.updateFoods(foods)

      val action = new ActionEatFood(world)
      val command = action.run()

      command.point mustEqual angleFoodPoint
    }
  }
}
