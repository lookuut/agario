package com.agario.navigation

import com.agario.{BaseSpec, world}
import com.agario.utils.Point

class FieldSpec extends BaseSpec {
  sequential
  "Chart spec" should {

    "Vector factor test1" in new world {
      val field = new BaseField()
      field.vectorFactor(new Point(61, 60), new Point(-47, 40))._2 mustEqual field.vectorFactor(new Point(14, 100), new Point(47, -40))._2
    }

    "Vector factor test2" in new world {
      val field = new BaseField(9, 1)

      field.vectorFactor(new Point(19, 20), new Point(1, 1))._1 mustEqual 1

      field.vectorFactor(new Point(19, 20), new Point(-19, -20))._1 mustEqual 5

      field.vectorFactor(new Point(19, 20), new Point(-19, -20))._2.size mustEqual 5

      field.vectorFactor(new Point(0,0), new Point(-20, -20))._1 mustEqual 0

      field.vectorFactor(new Point(0,0), new Point(20, 0))._1 mustEqual 3

      field.vectorFactor(new Point(0,0), new Point(19, 20))._1 mustEqual 5

      field.vectorFactor(new Point(30, 0), new Point(-30, 0))._1 mustEqual 4

      field.vectorFactor(new Point(8, 0), new Point(-8,0))._1 mustEqual 1

      field.vectorFactor(new Point(16, 0), new Point(-16,0))._1 mustEqual 2

      field.vectorFactor(new Point(17, 0), new Point(-17,0))._1 mustEqual 2

      field.vectorFactor(new Point(0,0), new Point(30, 0))._1 mustEqual 4

      field.vectorFactor(new Point(0,0), new Point(8, 0))._1 mustEqual 1

      field.vectorFactor(new Point(0,0), new Point(16, 0))._1 mustEqual 2

      field.vectorFactor(new Point(0,0), new Point(17, 0))._1 mustEqual 2
    }
  }
}
