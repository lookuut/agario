package com.agario.utils

import com.agario.BaseSpec


class LineSpec extends BaseSpec {

  "Line test" should {

    "Test tangient circle" in {
      val square = Line.tangentCircle(new Point(0,1), new Point(1,0), new Point(1,0), new Point(0,-1)).get.square()
      (
        (
          square
          <=
          new Circle(new Point(0,0), 1).square()
          &&
          square >= new Circle(new Point(0,0), 0.9999).square()
          )
          mustEqual true
        )


      val square1 = Line.tangentCircle(new Point(0,2), new Point(2,0), new Point(2,0), new Point(0,-2)).get.square()
      (
        (
          square1
            <=
            new Circle(new Point(0,0), 2).square()
            &&
            square1 >= new Circle(new Point(0,0), 0.9999).square()
          )
          mustEqual true
        )

      val square2 = Line.tangentCircle(new Point(1,2), new Point(1,0), new Point(2,1), new Point(0, 1)).get.square()
      (
        (
          square2
            <=
            new Circle(new Point(1,1), 1).square()
            &&
            square2 >= new Circle(new Point(1,1), 0.9999).square()
          )
          mustEqual true
        )
    }


    "Test line" in {
      val line = new Line(2, 1, -10)

      line.pointPos(new Point(10, 10)) mustEqual 1
      line.pointPos(new Point(10, -100000)) mustEqual -1
      line.pointPos(new Point(0, 10)) mustEqual 0
      line.pointPos(new Point(5, 0)) mustEqual 0
      line.pointPos(new Point(5, 1)) mustEqual 1
    }

    "Cross lines test" in {
      Line.intersect(new Point(0, 0), new Point(19, 20), new Point(0, 9), new Point(990, 9)).isDefined mustEqual true

      Line.intersect(new Point(0, 0), new Point(20, 20), new Point(20, 0), new Point(0, 20)).isDefined mustEqual true
      Line.intersect(new Point(0,0), new Point(0, 20), new Point(20, 0), new Point(20, 20)).isEmpty mustEqual true
      Line.intersect(new Point(0, 0), new Point(20, 0), new Point(10, 10), new Point(10, -20)).get mustEqual new Point(10, 0)
      Line.intersect(new Point(0, 0), new Point(21, 0), new Point(20, 0), new Point(30, 0)).get mustEqual new Point(Double.PositiveInfinity, Double.PositiveInfinity)
      Line.intersect(new Point(0, 0), new Point(40, 0), new Point(20, -1), new Point(500, 400)).isDefined mustEqual true
    }

    "Cross lines test" in {
      new Line(2, -1, 3).intersect(new Line(-3, -1, -2)).get mustEqual new Point (-1, 1)
      new Line(2, 2, 2).intersect(new Line(4, 4, 4)) mustEqual None
      new Line(5, 4, 4).intersect(new Line(2, 2, 2)).get mustEqual new Point(0, -1)
    }
  }
}
