package com.agario.actions

import com.agario.BaseSpec
import com.agario.utils.Point

class ActionMoveSpec extends BaseSpec{

  "Action move spec" should {

    "Goto food test with enemy" in {
      val world = getWorld()

      val playerWeight = 120
      val fWeight = 40
      val foodPos = new Point(30, 50)
      val fragmentPos = new Point(50, 50)
      val playerPos1 = new Point(5, 50)
      val playerPos2 = new Point(10, 50)

      val eFood = getEFood(foodPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, fWeight)
      val ePlayer = getEPlayer(playerPos1, playerWeight)

      world.updateWorld(Array(eFragment), Array(eFragment, eFood, ePlayer), 0)
      world.updateWorld(Array(eFragment), Array(eFragment, eFood,  getEPlayer(playerPos2, playerWeight)), 1)

      val fragment = world.getFragment(eFragment.getId)

      val action = new ActionMove(world)
      val move = action.run()

      (move.point- fragmentPos).x > 0 mustEqual true
    }

    "Goto ejection" in {
      val world = getWorld()

      val weight = 40
      val ejectionPos = new Point(30, 50)
      val fragmentPos = new Point(50, 50)

      val eEjection = getEEjection(ejectionPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, weight)

      world.updateWorld(Array(eFragment), Array(eFragment, eEjection), 0)
      world.updateWorld(Array(eFragment), Array(eFragment, getEEjection(ejectionPos + new Point(5, 0))), 1)
      val fragment = world.getFragment(eFragment.getId)
      println(fragment.getFieldsSum())

      val action = new ActionMove(world)
      val move = action.run()

      (move.point - fragmentPos).x < 0 mustEqual true
    }

    "Goto virus" in {
      val world = getWorld()

      val weight = 119
      val virusPos = new Point(30, 50)
      val fragmentPos = new Point(50, 50)

      val eVirus = getEVirus(virusPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, weight)

      world.updateWorld(Array(eFragment), Array(eFragment, eVirus), 0)

      val action = new ActionMove(world)
      val move = action.run()

      (move.point - fragmentPos).x > 0 mustEqual true
    }

    "Goto virus" in {
      val world = getWorld()

      val weight = 400
      val virusPos = new Point(30, 50)
      val fragmentPos = new Point(50, 50)

      val eVirus = getEVirus(virusPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, weight)

      world.updateWorld(Array(eFragment), Array(eFragment, eVirus), 0)

      val action = new ActionMove(world)
      val move = action.run()

      (move.point - fragmentPos).x < 0 mustEqual true
    }

    "Goto food test with enemy" in {
      val world = getWorld()

      val playerWeight = fragmentWeight
      val fWeight = 120
      val foodPos = new Point(30, 50)
      val fragmentPos = new Point(50, 50)
      val playerPos1 = new Point(5, 50)
      val playerPos2 = new Point(10, 50)

      val eFood = getEFood(foodPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, fWeight)
      val ePlayer = getEPlayer(playerPos1, playerWeight)

      world.updateWorld(Array(eFragment), Array(eFragment, eFood, ePlayer), 0)
      world.updateWorld(Array(eFragment), Array(eFragment, eFood,  getEPlayer(playerPos2, playerWeight)), 1)

      val action = new ActionMove(world)
      val move = action.run()

      (move.point- fragmentPos).x < 0 mustEqual true
    }

    "Goto food test" in {
      val world = getWorld()

      val foodPos = new Point(25f, 25f)
      val fragmentPos = new Point(50, 50)
      val eFood = getEFood(foodPos)
      val eFragment = getEFragment(fragmentPos, Point.zero)

      world.updateWorld(Array(eFragment), Array(eFragment, eFood), 0)

      val action = new ActionMove(world)
      val move = action.run()
      move.point.distance(eFragment.point) <= foodPos.distance(fragmentPos) mustEqual true
    }
  }
}
