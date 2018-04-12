package com.agario.models

import com.agario.BaseSpec
import com.agario.navigation.FragmentField
import com.agario.utils.Point

class FragmentSpec extends BaseSpec{
  "Fragment test" should {


    "World field" in {
      val world = getWorld()

      world.field.pointFactor(new Point(0, 0)) mustEqual FragmentField.edgeFactor
      world.field.pointFactor(new Point(1, 1)) mustEqual FragmentField.edgeFactor

      world.field.pointFactor(new Point(world.config.width - 1, world.config.height - 1)) mustEqual FragmentField.edgeFactor
      world.field.pointFactor(new Point(world.config.width - 1, 0)) mustEqual FragmentField.edgeFactor
      world.field.pointFactor(new Point(0, world.config.height - 1)) mustEqual FragmentField.edgeFactor

    }

    "Fragment Field" in {
      val world = getWorld()

      val eFragment = getEFragment(new Point(100, 100), Point.zero, 220)
      val eVirus = getEVirus(new Point(100,100))

      for (i <- 0 to 300) {
        val eFragment = getEFragment(new Point(100, 100 + i), Point.zero, 220)
        world.updateWorld(Array(eFragment), Array(eFragment, eVirus), i)
      }
      world.updateWorld(Array(getEFragment(new Point(100, 400), Point.zero, 220)), Array(getEFragment(new Point(100, 400), Point.zero, 220)), 301)

      world.getFragmentFieldSum().pointFactor(new Point(100, 400)) mustEqual 0
    }

    "Ejection virus food test" in {
      val world = getWorld()

      val ejectionPos = new Point(100, 130)
      val foodPos = new Point(130, 100)
      val virusPos = new Point(70, 100)

      val eEjection = getEEjection(ejectionPos)
      val eFood = getEFood(foodPos)
      val eVirus = getEVirus(virusPos)
      val eFragment = getEFragment(new Point(100, 100), Point.zero, 119)

      world.updateWorld(Array(eFragment), Array(eFragment, eEjection, eFood, eVirus), 0)

      world.getFragmentFieldSum().pointFactor(ejectionPos) < 0 mustEqual true
      world.getFragmentFieldSum().pointFactor(foodPos) < 0 mustEqual true
      world.getFragmentFieldSum().pointFactor(virusPos) == 0 mustEqual true
    }

    "Victim player" in {
      val world = getWorld()

      val fragmentPos = new Point(120, 120)
      val playerPos = new Point(100, 100)

      val ePlayer = getEPlayer(playerPos, 110)
      val eFragment = getEFragment(fragmentPos, Point.zero, 220)

      world.updateWorld(Array(eFragment), Array(eFragment, ePlayer), 0)
      val fragment = world.getFragment(eFragment.getId())
      fragment.getFieldsSum().pointFactor(playerPos) mustEqual -Player.playerFactor

      for (i <- 1 to 300) {
        val eFragment = getEFragment(fragmentPos, Point.zero, 220)
        world.updateWorld(Array(eFragment), Array(eFragment), i)
      }

      world.getFragmentFieldSum().pointFactor(playerPos) <= 0 mustEqual true
      world.getFragmentFieldSum().pointFactor(playerPos) < world.getFragmentFieldSum().pointFactor(playerPos + new Point(20, 20))
      world.getFragmentFieldSum().pointFactor(playerPos) < world.getFragmentFieldSum().pointFactor(playerPos + new Point(-20, -20))
      world.getFragmentFieldSum().pointFactor(playerPos) > -1 mustEqual true
    }

    "Predator player" in {
      val world = getWorld()

      val fragmentPos = new Point(120, 120)
      val playerPos = new Point(100, 120)

      val ePlayer = getEPlayer(playerPos, 220)
      val eFragment = getEFragment(fragmentPos, Point.zero, 110)

      world.updateWorld(Array(eFragment), Array(eFragment, ePlayer), 0)
      world.updateWorld(Array(eFragment), Array(eFragment, getEPlayer(playerPos + new Point(1,0), 220)), 1)
      val fragment = world.getFragment(eFragment.getId())

      fragment.getFieldsSum().pointFactor(playerPos) mustEqual Player.playerFactor

      for (i <- 1 to 300) {
        val eFragment = getEFragment(fragmentPos, Point.zero, 110)
        world.updateWorld(Array(eFragment), Array(eFragment), i)
      }

      world.getFragmentFieldSum().pointFactor(playerPos) <= 0 mustEqual true
    }
  }
}
