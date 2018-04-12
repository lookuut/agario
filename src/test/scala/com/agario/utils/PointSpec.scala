package com.agario.utils

import com.agario.BaseSpec

object PointSpec extends BaseSpec {
  "Point test" should {

    "Point angle test" in {

      var p1 = new Point(1, 1)
      var p2 = new Point(1, 0)

      println("=======" + p1.angleAgainstClockWay(p2))
      println("=======" + p2.angleAgainstClockWay(p1))
      true
    }

    "Mirror vector" in {
      new Point(3, 3).mirrorVector(new Point(10, 0)) mustEqual new Point(3, -3)
    }

    "Test multiple" in {
      new Point(3, 3) * 0.3 mustEqual new Point(3 * 0.3, 3 * 0.3)
      new Point(5, 5) * (1.0 / 3) mustEqual new Point(1 + 2.0 / 3, 1 + 2.0 / 3)
    }

    "Test reduce" in {
      Array(new Point(1,2), new Point(2,3), new Point(-10,10)).reduce(_ + _) mustEqual new Point(-7, 15)
    }

    "Test average" in {
      Array(new Point(1,2), new Point(2,3), new Point(-9, 10)).reduce(_ + _) * (1.0 / 3) mustEqual new Point(-2, 5)
    }

    "Test line" in {

      println(Point.line(new Point(0,0), new Point(20,20)))
      println(Point.line(new Point(100, 101), new Point(20,-20)))

      true
    }
  }
}
