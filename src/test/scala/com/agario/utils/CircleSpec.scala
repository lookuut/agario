package com.agario.utils

import com.agario.TestWorld
import com.agario.actions.ActionEscape

object CircleSpec extends org.specs2.mutable.Specification {
  "Circle spec" should {

    "Circle cover test" in {

      new Circle(new Point(0,0), 1).isCover(new Circle(new Point(0,1),1)) mustEqual false
      new Circle(new Point(0,0), 2).isCover(new Circle(new Point(0,1),1)) mustEqual false
      new Circle(new Point(1,0), 1).isCover(new Circle(new Point(1,0),1), 0.9999) mustEqual true
      new Circle(new Point(0,0), 3).isCover(new Circle(new Point(1,1),1)) mustEqual true
      new Circle(new Point(10, 10), 10).isCover(new Circle(new Point(0, 0),1)) mustEqual false
    }

    "Circle can cover test" in {
      val world = new TestWorld()
      new Circle(new Point(10, 10), 10).canCover(new Circle(new Point(10, 1), 1), world.getWorld()) mustEqual true
      new Circle(new Point(10, 10), 10).canCover(new Circle(new Point(1, 10), 1), world.getWorld()) mustEqual true
      new Circle(new Point(10, 10), 10).canCover(new Circle(new Point(10, 10), 1), world.getWorld()) mustEqual true
      new Circle(new Point(10, 10), 10).canCover(new Circle(new Point(5, 5), 1), world.getWorld()) mustEqual true

      new Circle(new Point(10, 10), 10).canCover(new Circle(new Point(1,1), 1), world.getWorld()) mustEqual false
      new Circle(new Point(100, 100), 10).canCover(new Circle(new Point(1, 1), 1), world.getWorld()) mustEqual false
      new Circle(new Point(2, 2), 2).canCover(new Circle(new Point(1, 1), 1), world.getWorld()) mustEqual false
      new Circle(new Point(3, 3), 3).canCover(new Circle(new Point(world.getWorld().config.width - 1,  world.getWorld().config.height - 1), 1), world.getWorld()) mustEqual false
    }

    "Is track intersect1" in {
      val victimTrack = Map(
        0 -> new Point(0, 0),
        1 -> new Point(1, 0),
        2 -> new Point(2, 0),
        3 -> new Point(3, 0)
      )

      val predatorTrack = Map(
        0 -> new Point(3, 3),
        1 -> new Point(3, 2),
        2 -> new Point(3, 1),
        3 -> new Point(3, 0)
      )

      Circle.isIntersectTracks(new Circle(new Point(0, 0), 1), victimTrack, new Circle(new Point(3, 3), 1), predatorTrack, ActionEscape.predatorDiameterFactor) mustEqual true
    }

    "Is track intersect2" in {
      val victimTrack = Map(
        0 -> new Point(0, 0),
        1 -> new Point(1, 0),
        2 -> new Point(2, 0),
        3 -> new Point(3, 0)
      )

      val predatorTrack = Map(
        0 -> new Point(3, 3),
        1 -> new Point(3, 2),
        2 -> new Point(3, 1),
        3 -> new Point(3, 0)
      )

      Circle.isIntersectTracks(new Circle(new Point(0, 0), 1), victimTrack, new Circle(new Point(3, 3), 1), predatorTrack, 1.0) mustEqual false
    }

    "Is track intersect3" in {
      val victimTrack = Map(
        0 -> new Point(0, 0),
        1 -> new Point(1, 0),
        2 -> new Point(2, 0),
        3 -> new Point(3, 0)
      )

      val predatorTrack = Map(
        0 -> new Point(3, 3),
        1 -> new Point(3, 2),
        2 -> new Point(3, 1),
        3 -> new Point(5, 0)
      )

      Circle.isIntersectTracks(new Circle(new Point(0, 0), 2), victimTrack, new Circle(new Point(3, 3), 2), predatorTrack, 2.0f / 3) mustEqual false
    }
  }
}
