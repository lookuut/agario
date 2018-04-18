package com.agario.models

import com.agario.{BaseSpec, world}
import com.agario.navigation.BaseField
import com.agario.utils.{Circle, Point, Trajectory}
import com.agario.navigation.FadingField

class FragmentSpec extends BaseSpec{
  sequential
  "Fragment test" should {

    "Fade food" in new world {
      val foodPos = new Point(130, 100)

      val eFood = getEFood(foodPos)
      val eFragment = getEFragment(new Point(100, 100), Point.zero, 119)

      World.updateWorld(Array(eFragment), Array(eFragment, eFood), 0)

      World.updateWorld(Array(eFragment), Array(getEFragment(new Point(80, 100), Point.zero, 119)), 1)

      World.getFieldsSum().pointFactor(foodPos) < 0 mustEqual true
    }



    "World field" in new world {
      World.getFieldsSum().pointFactor(new Point(0, 0)) < 0 mustEqual true
      World.getFieldsSum().pointFactor(new Point(1, 1))  mustEqual FadingField.getStartFadingFactor(BaseField.defaultPropose, World.chartWidth, World.chartHeight)
    }

    "Fragment Field" in new world {

      val eFragment = getEFragment(new Point(100, 100), Point.zero, 220)
      val eVirus = getEVirus(new Point(100,100))

      for (i <- 0 to 300) {
        val eFragment = getEFragment(new Point(100, 100 + i), Point.zero, 220)
        World.updateWorld(Array(eFragment), Array(eFragment, eVirus), i)
      }
      World.updateWorld(Array(getEFragment(new Point(100, 400), Point.zero, 220)), Array(getEFragment(new Point(100, 400), Point.zero, 220)), 301)

      World.getFieldsSum().pointFactor(new Point(100, 400)) > FadingField.getStartFadingFactor(BaseField.defaultPropose, World.chartWidth, World.chartHeight) mustEqual true
    }

    "Ejection virus food test" in new world {
      val ejectionPos = new Point(100, 130)
      val foodPos = new Point(130, 100)
      val virusPos = new Point(500, 500)

      val eEjection = getEEjection(ejectionPos)
      val eFood = getEFood(foodPos)
      val eVirus = getEVirus(virusPos)
      val eFragment = getEFragment(new Point(100, 100), Point.zero, 119)

      World.updateWorld(Array(eFragment), Array(eFragment, eEjection, eFood, eVirus), 0)

      World.getFieldsSum().pointFactor(ejectionPos) < 0 mustEqual true
      World.getFieldsSum().pointFactor(foodPos) < 0 mustEqual true
      World.getFieldsSum().pointFactor(virusPos) mustEqual FadingField.getStartFadingFactor(BaseField.defaultPropose, World.chartWidth, World.chartHeight)
    }

    "Victim player" in new world {
      val fragmentPos = new Point(120, 120)
      val playerPos = new Point(400, 400)

      val ePlayer = getEPlayer(playerPos, 110)
      val eFragment = getEFragment(fragmentPos, Point.zero, 220)

      World.updateWorld(Array(eFragment), Array(eFragment, ePlayer), 0)
      val fragment = World.getFragment(eFragment.getId())
      fragment.getFieldsSum().pointFactor(playerPos) mustEqual -Player.playerFactor

      println(World.getFieldsSum())
      println("========>")

      for (i <- 1 to 100) {
        World.updateWorld(Array(getEFragment(fragmentPos, Point.zero, 220)), Array(getEFragment(fragmentPos, Point.zero, 220)), 1)
      }

      World.updateWorld(Array(getEFragment(playerPos, Point.zero, 220)), Array(getEFragment(playerPos, Point.zero, 220)), 101)


      World.getFieldsSum().pointFactor(playerPos) <= 0 mustEqual true
      World.getFieldsSum().pointFactor(playerPos) < World.getFieldsSum().pointFactor(playerPos + new Point(20, 20))
      World.getFieldsSum().pointFactor(playerPos) < World.getFieldsSum().pointFactor(playerPos + new Point(-20, -20))
      World.getFieldsSum().pointFactor(playerPos) > -1 mustEqual true
    }

    "Predator player1" in new world {

      val fragmentPos = new Point(100, 120)
      val playerPos = new Point(400, 120)
      val playerDelta = new Point(-5, 0)

      val ePlayer = getEPlayer(playerPos, 220)
      val eFragment = getEFragment(fragmentPos, Point.zero, 110)

      World.updateWorld(Array(eFragment), Array(eFragment, ePlayer), 0)
      World.updateWorld(Array(eFragment), Array(eFragment, getEPlayer(playerPos + playerDelta, 220)), 1)
      val fragment = World.getFragment(eFragment.getId())

      fragment.getFieldsSum().pointFactor(new Point(0, 181)) > 0 mustEqual true
    }

    "Predator player2" in new world {
      val fragmentPos = new Point(120, 100)
      val playerPos = new Point(120, 400)
      val playerDelta = new Point(0, -5)

      val ePlayer = getEPlayer(playerPos, 220)
      val eFragment = getEFragment(fragmentPos, Point.zero, 110)

      World.updateWorld(Array(eFragment), Array(eFragment, ePlayer), 0)
      World.updateWorld(Array(eFragment), Array(eFragment, getEPlayer(playerPos + playerDelta, 220)), 1)
      val fragment = World.getFragment(eFragment.getId())
      fragment.getFieldsSum().pointFactor(new Point(121, 0)) > 0 mustEqual true
    }

    "Predator player3" in new world {
      val width = World.config.width
      val height = World.config.height

      val fragmentPos = new Point(width - 120, height - 60)
      val playerPos = new Point(width - 120, height - 400)
      val playerDelta = new Point(0, 5)

      val ePlayer = getEPlayer(playerPos, 220)
      val eFragment = getEFragment(fragmentPos, Point.zero, 110)

      World.updateWorld(Array(eFragment), Array(eFragment, ePlayer), 0)
      World.updateWorld(Array(eFragment), Array(eFragment, getEPlayer(playerPos + playerDelta, 220)), 1)
      val fragment = World.getFragment(eFragment.getId())

      fragment.getFieldsSum().pointFactor(new Point(width - 120, height - 1)) > 0 mustEqual true
    }

    "Predator player4" in new world {
      val width = World.config.width
      val height = World.config.height

      val fragmentPos = new Point(120, height - 60)
      val playerPos = new Point(120, height - 400)
      val playerDelta = new Point(0, 5)

      val ePlayer = getEPlayer(playerPos, 220)
      val eFragment = getEFragment(fragmentPos, Point.zero, 110)

      World.updateWorld(Array(eFragment), Array(eFragment, ePlayer), 0)
      World.updateWorld(Array(eFragment), Array(eFragment, getEPlayer(playerPos + playerDelta, 220)), 1)
      val fragment = World.getFragment(eFragment.getId())

      fragment.getFieldsSum().pointFactor(new Point(120, height - 1)) > 0  mustEqual true
    }
  }
}
