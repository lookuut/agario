package com.agario

import com.agario.actions.ActionManager
import com.agario.models._
import com.agario.utils.Point

import scala.collection.mutable
import scala.collection.mutable.HashMap


class Strategy() {

  val actionManager = new ActionManager()

  def tick(fragments : Array[Entity],
           entities : Array[Entity],
            tick : Int)
  : Response = {

    World.updateWorld(fragments, entities, tick)
    actionManager.run()
  }
}

object Strategy {

  val xDir = new Point(1, 0)
  val trajectories = HashMap.empty[Int, Map[Int, (Double, Double)]]//weight tick, start speed factor and series sum
  val speeds = HashMap.empty[Int, HashMap[Int, (Double, Double)]]//weight tick, start speed factor and series sum
  val maxPredictionTick = 50
  val speedDiscreteCount = 20
  val angleDiscreteCount = 20
  val maxWeightToPrediction = 120

  val angleD = 2 * math.Pi / angleDiscreteCount
  val speedD = BaseEntity.maxSpeed(Fragment.startWeight) / speedDiscreteCount


  val prediction = Array.fill[
                        HashMap[Point,
                            HashMap[
                              Int, Array[Point]
                              ]
                            ]
    ](maxWeightToPrediction - Fragment.startWeight.toInt)(HashMap.empty[Point, HashMap[Int,Array[Point]]] ) //weight, pos ,start speed, tick , direction

  val discreteSpeeds = (0 to speedDiscreteCount).map{
    case speedPart =>

      BaseEntity.maxSpeed(Fragment.startWeight) / speedDiscreteCount
  }

  val angleSectors = {
    (0 to angleDiscreteCount - 1).map{
      case angle =>
        val v = xDir.turn(2 * math.Pi * angle / angleDiscreteCount)
        xDir.angleAgainstClockWay(v)
    }
  }


  def getPosAtTick(weight : Double, tick : Int, startSpeed : Point, direction : Point) : Option[Point] = {
    if (trajectories.get(weight.floor.toInt).isEmpty || trajectories.get(weight.floor.toInt).get.get(tick).isEmpty) {
      return None
    }

    val factor = trajectories.get(weight.floor.toInt).get.get(tick).get

    val pos = new Point(
      startSpeed.x * factor._1 + direction.x * factor._2,
      startSpeed.y * factor._1 + direction.y * factor._2
    )

    Some(pos)
  }

  def speedFactor(weight : Double, tick : Int) : Double = {
    math.pow((1 - BaseEntity.inertion(weight)), tick)
  }

  def directionFactor(weight : Double, tick : Int, inertia : Double, maxSpeed : Double) : Double = {
    var dirFactor = 0.0

    for (speedIter <- 0 to tick - 1) {
      if (speedIter >= 0) {
        dirFactor += math.pow((1 - inertia) , (speedIter))
      }
    }

    maxSpeed * inertia * dirFactor
  }

  def getSpeedAtTick(weight : Double, tick : Int, startSpeed : Point, direction : Point ): Option[Point] = {
    if (speeds.get(weight.floor.toInt).isEmpty || speeds.get(weight.floor.toInt).get.get(tick).isEmpty) {
      return None
    }

    val factor = speeds.get(weight.floor.toInt).get.get(tick).get
    Some(startSpeed * factor._1  +  direction * BaseEntity.maxSpeed(weight) * BaseEntity.inertion(weight) * factor._2)
  }

  def preload(config : Config) : String = {

    for (weight <- Fragment.startWeight.toInt until maxWeightToPrediction)  {
      val inertia = BaseEntity.inertion(weight)
      val maxSpeed = BaseEntity.maxSpeed(weight)

      val predicatedSpeedParams = HashMap.empty[Int, (Double, Double)]
      var dirFactor = 0.0
      var startSpeedFactor = 0.0

      val predicatedPosParams = (for (tick <- 1 to maxPredictionTick) yield {
        var speedFactor = 0.0
        for (i <- 0 to tick - 1) {
          speedFactor += math.pow((1 - inertia) , i)
        }
        predicatedSpeedParams.put(tick, (math.pow((1 - inertia), tick) , speedFactor))

        startSpeedFactor += math.pow((1 - inertia), tick)

        for (speedIter <- 0 to tick - 1) {
          if (speedIter >= 0) {
            dirFactor += math.pow((1 - inertia) , (speedIter))
          }
        }

        (tick, (startSpeedFactor, maxSpeed * inertia * dirFactor))
      }).toMap

      speeds.put(weight, predicatedSpeedParams)
      trajectories.put(weight, predicatedPosParams)

      for (speedPart <- 1 to speedDiscreteCount) {
        val startSpeedPower = speedPart * speedD
        val startSpeed = new Point(startSpeedPower, 0)
          angleSectors.foreach {
            case directionAngle =>
              val direction = xDir.turn(directionAngle).normalize()

              for (tick <- 1 to maxPredictionTick) {
                val pos = getPosAtTick(weight, tick, startSpeed, direction).get
                addPrediction(getWeightIndex(weight), getPosId(pos, startSpeed), getSpeedId(startSpeed), tick, direction)
              }
          }
      }

      (weight, predicatedPosParams)
    }

    "Success"
  }

  def getPosId(pos : Point, speed : Point): Point = {

    val angle = (speed.angleAgainstClockWay(pos) / angleD)

    new Point(
      (pos.length() / 2 + minDela).floor.toInt
      ,
      if (angle < 0) {
        (angle - minDela).ceil.toInt
      } else {
        (angle + minDela).floor.toInt
      }
    )
  }

  def getSpeedById(speedId : Point): Point = {

    xDir.turn(speedId.y) * speedId.x * speedD
  }

  def getDiscreteSpeed(point : Point): Point = {

    val a = (xDir.angleAgainstClockWay(point) / angleD)
    val angle =
      if (a < 0) {
        (a - minDela).ceil.toInt
      } else {
        (a + minDela).floor.toInt
      }

    val l = (point.length() / speedD + minDela).floor.toInt

    xDir.turn(angle * angleD) * (l * speedD)
  }

  val minDela = 0.0000001
  def getSpeedId(speed : Point) : Int = {
    (speed.length() / speedD + minDela).floor.toInt
  }

  def getWeightIndex(weight : Double) : Int = (weight.floor.toInt - Fragment.startWeight.toInt)


  def addPrediction(weight : Int, pos : Point, speedId: Int, tick : Int, direction : Point) : Unit = {

    if (prediction(weight).get(pos).isEmpty) {
      prediction(weight).put(pos, HashMap.empty[Int, Array[Point]])
    }

    if (prediction(weight).get(pos).get.get(speedId).isEmpty) {
      prediction(weight).get(pos).get.put(speedId, Array.fill[Point](maxPredictionTick)(Point.zero))
    }

    if (prediction(weight).get(pos).get.get(speedId).get(tick - 1) == Point.zero) {
      prediction(weight).get(pos).get.get(speedId).get(tick - 1) = direction
    }
  }

  def getPrediction(weight : Double, pos : Point, speed : Point) : Array[Point] = {
    val speedId = getSpeedId(speed)
    val weightIndex = getWeightIndex(weight)

    if (prediction.length <= weightIndex ||
      prediction(weightIndex).size == 0 ||
      prediction(weightIndex).get(pos).isEmpty ||
      prediction(weightIndex).get(pos).get.get(speedId).isEmpty) {


      Array.empty[Point]
    } else {
      prediction(weightIndex).get(pos).get.get(speedId).get
    }
  }

  def getPredictionMinTick (startSpeed : Point, predictions : Iterable[Point]) : (Int, Point) = {
    var tick = 1
    for (point <- predictions) {
      if (point != Point.zero) {
         return (tick, getDirection(startSpeed, point))
      }
      tick +=1
    }

    return (tick, Point.zero)
  }

  def getDirection(speed : Point, point : Point) : Point = {
    point.turn(speed.angle(xDir))
  }

}

