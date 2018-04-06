package com.agario.actions

import com.agario.commands.{Command, Empty, Move}
import com.agario.models.{Food, Fragment, Track, World}
import com.agario.utils.{Circle, Point, Trajectory}

class ActionEatFood(world : World) extends Action {

  var food : Option[Point] = None
  var toFoodTrack : Option[Track] = None
  var foodFindTick = 0

  val fragmentsStartPoints = world.fragments.values.map(f => (f.id, f.circle.point)).toMap

  var findFoodTick = -1
  var findFoodLastResult : Option[Point] = None

  findFood()

  val distances = fragmentsStartPoints.values.map(p => p.distance(findFoodLastResult.getOrElse(Point.zero())))
  var distanceToFood = if (distances.size > 0) distances.min else 0

  def findFood (): Option[Point] = {
    if (findFoodTick == world.tick) {
      return findFoodLastResult
    }

    val foods = world.foods.values

    if (foods.size == 0) {
      return None
    }

    val minDistanceFood = world.fragments.values.
      map {
        case f =>
          val minDistanceFood = foods.
            filter(food => f.circle.canCover(food.circle, world)).
            filter(food => food.circle.point.x >= f.circle.r &&
              food.circle.point.y >= f.circle.r &&
              food.circle.point.x <= world.config.width - f.circle.r &&
              food.circle.point.y <= world.config.height - f.circle.r).
            map(t =>
              (
                Fragment.positionTick(f, t.circle.point, world.config)._1,
                t
              )
            )

          if (minDistanceFood.size > 0)
            (minDistanceFood.size, minDistanceFood.minBy(_._1)._1, minDistanceFood.minBy(_._1)._2, f)
          else
            (minDistanceFood.size, 0, new Food(Circle.zero(), 0), f)

      }.
      filter(_._1 > 0)

    if (minDistanceFood.size > 0)  {
      findFoodLastResult = Some(minDistanceFood.minBy(_._2)._3.circle.point)
    } else {
      findFoodLastResult = None
    }

    findFoodTick = world.tick
    findFoodLastResult
  }

  def run () : Command = {
    val nearestFood = findFood()


    if (nearestFood.isDefined) {

      val toNearestTrack = Trajectory.searchTrack(
        world,
        world.getMinDistanceFragment(nearestFood.get).get,
        new Circle(nearestFood.get, Food.radius),
        Food.coverPart)


      if (food.isDefined && nearestFood != food) {

        if (toFoodTrack.get.getEndTick() > toNearestTrack.getEndTick()) {
          food = nearestFood
          toFoodTrack = Some(toNearestTrack)
          foodFindTick = world.tick
        }
      } else {
        foodFindTick = world.tick
        food = nearestFood
        toFoodTrack = Some(toNearestTrack)
      }
    }

    if (food.isDefined) {
      new Move(food.get, toFoodTrack.get)
    } else
      new Empty(Point.zero)
  }

  def isEnd(): Boolean = {
    if (world.fragments.size > 1) {

      fragmentsStartPoints.filter{
        case (fragmentId, point) =>
          val fragment = world.fragments.get(fragmentId)
          fragment.isDefined && fragment.get.circle.point.distance(point) > distanceToFood
      }.size > 0

    } else {
      !food.isDefined
    }
  }
}


