package com.agario.navigation

import com.agario.BaseSpec
import com.agario.utils.{Point}

class FieldSpec extends BaseSpec {

  "Chart spec" should {

    "Vector factor test1" in {
      val world = getWorld()
      val field = new FadingField(world, 1, 6)
      field.vectorFactor(new Point(61, 60), new Point(-17, 10))._2 mustEqual field.vectorFactor(new Point(44, 70), new Point(17, -10))._2

      //field.vectorFactor(new Point(60, 60), new Point(19, 57))._1 mustEqual 5
    }

    "Vector factor test2" in {
      val world = getWorld()
      val field = new FadingField(world, 1, 9)

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
