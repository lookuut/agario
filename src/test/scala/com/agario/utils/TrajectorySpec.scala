package com.agario.utils

import com.agario.TestWorld
import com.agario.models.Fragment

class TrajectorySpec extends org.specs2.mutable.Specification {

  val world = new TestWorld().getWorld()

  val weight = 40
  val radius = 10
  val maxSpeed = world.config.speedFactor / math.sqrt(weight)

  val fragment = new Fragment("1", new Circle(new Point(0, 0), radius), weight, new Point(maxSpeed, 0), None)
  val coverPart = 2.0f / 3
  "Trajectory" should {

    "Trajectory test 1" in {

      val target = new Circle(new Point(100, 100), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 2" in {
      val target = new Circle(new Point(100, 0), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 3" in {
      val target = new Circle(new Point(0, 100), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 4" in {
      val target = new Circle(new Point(-100, 100), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 5" in {
      val target = new Circle(new Point(100, 300), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 6" in {
      val target = new Circle(new Point(200, 200), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 7" in {
      val target = new Circle(new Point(1000, 0), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 8" in {
      val target = new Circle(new Point(0, 0), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }
  }
}
