package com.agario.utils

import com.agario.BaseSpec
import com.agario.models.Fragment
import com.agario.utils.CircleSpec.getWorld

class TrajectorySpec extends BaseSpec {
  val coverPart = 2.0f / 3
  "Trajectory" should {

    "Trajectory test 0" in {
      val fragment = getFragment(new Point(200.63489713918028, 222.33456082826868), new Point(1.0760983375945607, 2.120892519030835), 42)
      val world = getWorld()
      val target = new Circle(new Point(247.0, 263.0), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 1" in {
      val fragment = getFragment(new Point(0, 0), new Point(3, 0), fragmentWeight)
      val world = getWorld()
      val target = new Circle(new Point(100, 100), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 2" in {
      val world = getWorld()
      val fragment = getFragment(new Point(0, 0), new Point(3, 0), fragmentWeight)
      val target = new Circle(new Point(100, 0), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 3" in {
      val world = getWorld()
      val target = new Circle(new Point(0, 100), 2.5)
      val fragment = getFragment(new Point(0, 0), new Point(3, 0), fragmentWeight)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 4" in {
      val world = getWorld()
      val target = new Circle(new Point(200, 100), 2.5)
      val fragment = getFragment(new Point(0, 0), new Point(3, 0), fragmentWeight)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 5" in {
      val world = getWorld()
      val target = new Circle(new Point(100, 300), 2.5)
      val fragment = getFragment(new Point(0, 0), new Point(3, 0), fragmentWeight)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 6" in {
      val world = getWorld()
      val target = new Circle(new Point(200, 200), 2.5)
      val fragment = getFragment(new Point(0, 0), new Point(3, 0), fragmentWeight)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 7" in {
      val world = getWorld()
      val fragment = getFragment(new Point(0, 0), new Point(3, 0), fragmentWeight)
      val target = new Circle(new Point(1000, 0), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }

    "Trajectory test 8" in {
      val world = getWorld()
      val fragment = getFragment(new Point(0, 0), new Point(3, 0), fragmentWeight)
      val target = new Circle(new Point(0, 0), 2.5)
      var optimalTrack = Trajectory.searchTrack(world, fragment, target, coverPart)
      val simpleTrack = new Trajectory(world , fragment, target, coverPart).straight()

      optimalTrack.endTick <= simpleTrack.endTick mustEqual true
    }
  }
}
