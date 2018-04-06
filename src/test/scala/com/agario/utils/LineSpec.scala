package com.agario.utils



class LineSpec extends org.specs2.mutable.Specification {

  "Line test" should {
    "Test line" in {
      val line = new Line(2, 1, -10)

      line.pointPos(new Point(10, 10)) mustEqual 1
      line.pointPos(new Point(10, -100000)) mustEqual -1
      line.pointPos(new Point(0, 10)) mustEqual 0
      line.pointPos(new Point(5, 0)) mustEqual 0
      line.pointPos(new Point(5, 1)) mustEqual 1
    }
  }
}
