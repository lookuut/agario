package com.agario

import com.rojoma.json.v3.util.SimpleJsonCodecBuilder

class Config(val width : Int,
             val height : Int,
             val ticks : Int,
             val foodWeight : Double,
             val maxFragmentsCount : Int,
             val ticksTillFusion : Int,
             val virusRadius : Double,
             val virusSplitMass : Double,
             val viscosity : Double,
             val inertionFactor : Double,
             val speedFactor : Double
            ) {

}



object Config {
  implicit val jCodec = SimpleJsonCodecBuilder[Config].
    build(
      "GAME_WIDTH", _.width,
      "GAME_HEIGHT", _.height,
      "GAME_TICKS", _.ticks,
      "FOOD_MASS", _.foodWeight,
      "MAX_FRAGS_CNT", _.maxFragmentsCount,
      "TICKS_TIL_FUSION", _.ticksTillFusion,
      "VIRUS_RADIUS", _.virusRadius,
      "VIRUS_SPLIT_MASS", _.virusSplitMass,
      "VISCOSITY", _.viscosity,
      "INERTION_FACTOR", _.inertionFactor,
      "SPEED_FACTOR", _.speedFactor
    )

  val minWeightToBurst = 120
  val minTicksToVirus = 100
}