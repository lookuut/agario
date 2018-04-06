package com.agario.ml

import com.agario.utils.Point
import com.ml.genetic.GeneticBase

class Genetic(n : Int,
              featureMaxValue : Int,
              val intervalTicks: Int,
              val anglesCount : Int,
              val targetPoint : Point,
              val startSpeed : Point,
              val weight : Double) extends GeneticBase(n, featureMaxValue){

  val discreteAngles = (0 to anglesCount).map(t => (t * Math.PI * 2 / anglesCount, t)).toMap

  def populationPower(feature : Array[Int]): Double = {
    0.0
    /*
    feature.map{
        case feature =>
           featureToAngle(feature)
      }.reduce{
        case ()
      }*/
  }


  def featureToAngle(feature : Int) : Double = {
    discreteAngles.get(feature).get
  }

}
