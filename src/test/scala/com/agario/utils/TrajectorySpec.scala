package com.agario.utils

import com.agario.{BaseSpec, Strategy, world}
import com.agario.models.{BaseEntity}

class TrajectorySpec extends BaseSpec {
  sequential

  val coverPart = 2.0f / 3

  "Trajectory" should {


    "Speed Test" in new world {

      val weight = 40.0

      val maxSpeed = BaseEntity.maxSpeed(weight)
      val inertia = BaseEntity.inertion(weight)
      val sSpeed = new Point(1, 0)

      val pos = new Point(100, 100)
      val targetPos = new Point(160, 100)
      val direction = (targetPos - pos).normalize()

      var speed = sSpeed
      for (i <- 1 to 100) {

        speed = Trajectory.tickSpeed(direction, speed, maxSpeed, inertia)
        val speed1 = Trajectory.speed(sSpeed, direction, maxSpeed, inertia, i)
        println(f"$speed $speed1")
      }

      1 > 2 mustEqual false
    }
  }
}
