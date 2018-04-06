package com.ml.genetic

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random


class GeneticBase(val n : Int, val featureMaxValue : Int) {
  val features = ListBuffer()
  val random = new Random()


  def randomPopulation(count : Int) : Array[Array[Int]] = {
    (for (i <- 0 to count) yield {
      (for (j <- 0 until n) yield  {
        random.nextInt(featureMaxValue)
      }).toArray
    }).toArray
  }

  def selection (): Unit = {

  }

  def crossing() : Unit = {

  }

  def mutation (feature : ArrayBuffer[Double]) : ArrayBuffer[Double] = {
    feature(random.nextInt(n - 1)) = random.nextInt(featureMaxValue)
    feature
  }

  def crossover(mother : Array[Double], father : Array[Double], mothersPart : Double) : Array[Double] = {
    val size = mother.size
    val motherPartSize = (size * mothersPart).ceil.toInt

    (for (i <- 0 to size) yield {
      if (i <= motherPartSize) {
        mother(i)
      } else {
        father(i)
      }
    }).toArray
  }
}
