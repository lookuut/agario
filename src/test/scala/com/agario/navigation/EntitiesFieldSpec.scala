package com.agario.navigation

import com.agario.models.World
import com.agario.{BaseSpec, world}
import com.agario.utils.Point

class EntitiesFieldSpec extends BaseSpec {
  sequential
  "Chart spec" should {

    "Map with food test" in new world {
      val foodPos = new Point(130, 100)

      val eFood = getEFood(foodPos)
      val eFood1 = getEFood(new Point(130, 200))
      val eFragment = getEFragment(new Point(100, 100), Point.zero, 40)

      for (i <- 1 to 150) {
        World.updateWorld(Array(eFragment), Array(eFragment, eFood), i)
      }

      World.updateWorld(Array(getEFragment(new Point(130, 790), Point.zero, 40)), Array(getEFragment(new Point(130, 790), Point.zero, 40)), 151)
      val field = new BaseField()

      println(World.staticEntitiesField.sum(field, 300))

      true
    }
  }
}
