package com.agario.models

import com.agario.{Config, TestWorld}
import com.agario.utils.{Circle, Point}

class FragmentSpec  extends org.specs2.mutable.Specification {
  "Fragment test" should {

    "Move with correction test" in {

      val world = new TestWorld().getWorld()
      val weight = 40
      val targetPoint = new Point(1050, 100)
      val maxSpeed = Fragment.maxSpeed(weight, world.config)
      val fragment = new Fragment("1", new Circle(new Point(100, 100), weight / math.sqrt(40)), weight, new Point(maxSpeed / math.sqrt(2), maxSpeed / math.sqrt(2)), None)
      val fragments = Map("1" -> fragment)


      world.updateFragments(fragments)

      val straightMoveTick = Fragment.positionTick(fragment, targetPoint, world.config)._1
      val moveWithCorrectionTick = Fragment.moveWithCorrectionTick(fragment, targetPoint, world.config)._1

      println(f"$straightMoveTick $moveWithCorrectionTick")

      straightMoveTick > moveWithCorrectionTick
    }
  }
}
