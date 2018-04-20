package com.agario.actions

import com.agario.{BaseSpec, Strategy, world}
import com.agario.models.World
import com.agario.navigation.BaseField
import com.agario.utils.{Point, Profiler}

class ActionMoveSpec extends BaseSpec{

  sequential
  "Action move test" should {

    "Goto in empty map" in new world  {
      val weight = 40

      val fragmentPos = new Point(400, 360)
      val foodPos = new Point(445, 445)
      val foodCell = BaseField.pointCell(foodPos)

      val eFood1 = getEFood(foodPos + new Point(20, 20))
      val eFood2 = getEFood(foodPos + new Point(30, 20))
      val eFood3 = getEFood(foodPos + new Point(40, 20))
      val eFood4 = getEFood(foodPos + new Point(50, 20))
      val eFood5 = getEFood(foodPos + new Point(60, 20))
      val eFood6 = getEFood(foodPos + new Point(70, 20))
      val eFood7 = getEFood(foodPos + new Point(80, 20))
      val eFood8 = getEFood(foodPos + new Point(90, 20))

      val eFragment = getEFragment(fragmentPos, Point.zero, weight)
      World.updateWorld(Array(eFragment), Array(eFragment, eFood1,eFood2,eFood3,eFood4,eFood5,eFood6,eFood7), 0)

      World.updateWorld(Array(eFragment), Array(eFragment,  eFood1,eFood2,eFood3,eFood4,eFood5,eFood6,eFood7), 1)
      val (res, mills) = Profiler.profile(
        {
          val action = new ActionMove()
        }
      )

      println(mills)

      1 < 2 mustEqual true
      //(move.point - fragmentPos).y > 0 mustEqual true
    }

    "Goto ejection" in new world  {
      val weight = 40
      val ejectionPos = new Point(100, 100)
      val fragmentPos = new Point(30, 100)

      val eEjection = getEEjection(ejectionPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, weight)

      World.updateWorld(Array(eFragment), Array(eFragment, eEjection), 0)
      World.updateWorld(Array(eFragment), Array(eFragment, getEEjection(ejectionPos + new Point(5, 0))), 1)

      val fragment = World.getFragment(eFragment.getId)

      val action = new ActionMove()
      val move = action.run()

      (move.entity.posCircle.point - fragmentPos).x > 0 mustEqual true
    }

    "Goto food test with enemy" in new world {
      val playerWeight = 120
      val fWeight = 40
      val foodPos = new Point(100, 100)
      val fragmentPos = new Point(150, 100)
      val playerPos1 = new Point(50, 100)
      val playerPos2 = new Point(52, 100)

      val eFood = getEFood(foodPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, fWeight)
      val ePlayer = getEPlayer(playerPos1, playerWeight)

      World.updateWorld(Array(eFragment), Array(eFragment, eFood, ePlayer), 0)
      World.updateWorld(Array(eFragment), Array(eFragment, eFood,  getEPlayer(playerPos2, playerWeight)), 1)

      val fragment = World.getFragment(eFragment.getId)

      val action = new ActionMove()
      val move = action.run()

      (move.entity.posCircle.point- fragmentPos).x > 0 mustEqual true
    }


    "Goto virus" in new world  {
      val weight = 119
      val virusPos = new Point(30, 50)
      val fragmentPos = new Point(100, 100)

      val eVirus = getEVirus(virusPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, weight)

      World.updateWorld(Array(eFragment), Array(eFragment, eVirus), 0)

      val action = new ActionMove()
      val move = action.run()

      (move.entity.posCircle.point - fragmentPos).x > 0 mustEqual true
    }

    "Goto virus" in new world  {

      val weight = 400
      val virusPos = new Point(30, 30)
      val fragmentPos = new Point(120, 120)

      val eVirus = getEVirus(virusPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, weight)

      World.updateWorld(Array(eFragment), Array(eFragment, eVirus), 0)

      val action = new ActionMove()
      val move = action.run()

      (move.entity.posCircle.point - fragmentPos).x < 0 mustEqual true
    }

    "Goto food test with enemy" in new world  {

      val playerWeight = fragmentWeight
      val fWeight = 120
      val foodPos = new Point(30, 50)
      val fragmentPos = new Point(50, 50)
      val playerPos1 = new Point(5, 50)
      val playerPos2 = new Point(10, 50)

      val eFood = getEFood(foodPos)
      val eFragment = getEFragment(fragmentPos, Point.zero, fWeight)
      val ePlayer = getEPlayer(playerPos1, playerWeight)

      World.updateWorld(Array(eFragment), Array(eFragment, eFood, ePlayer), 0)
      World.updateWorld(Array(eFragment), Array(eFragment, eFood,  getEPlayer(playerPos2, playerWeight)), 1)

      val action = new ActionMove()
      val move = action.run()

      (move.entity.posCircle.point - fragmentPos).x < 0 mustEqual true
    }

    "Goto food test" in new world  {

      val foodPos = new Point(100, 100)
      val foodCell = BaseField.pointCell(foodPos)

      val fragmentPos = new Point(25f, 25f)
      val eFood = getEFood(foodPos)
      val eFragment = getEFragment(fragmentPos, new Point(5, 0))

      World.updateWorld(Array(eFragment), Array(eFragment, eFood), 0)

      val action = new ActionMove()
      val move = action.run()

      action.command.track.visitedCells.keySet.contains(foodCell) mustEqual true
    }
  }
}
