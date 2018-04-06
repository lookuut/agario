package com.agario.models

import com.agario.Config
import com.agario.navigation.Chart
import com.agario.utils.Point

class World(val config : Config) {

  val chartWidth = (config.width / Chart.propose).floor.toInt
  val chartHeight = (config.height / Chart.propose).floor.toInt

  val mapCenter = new Point(config.width / 2, config.height / 2)
  var tick = 0
  var fragments = Map.empty[String, Fragment]
  var viruses = Map.empty[String, Virus]
  var foods = Map.empty[Point, Food]
  var players = Map.empty[String, Player]
  var ejections = Array.empty[Ejection]

  var isWorldChanged = false
  val chart = new Chart(this)

  def updateWorld (_fragments : Map[String, Fragment],
                   _viruses : Map[String , Virus],
                   _foods : Map[Point, Food],
                   _ejections : Array[Ejection],
                   _players : Map[String , Player],
                   _tick : Int
                    ): Unit = {

    val prevEjectionIds = ejections.map(t => t.pId).toSet
    val prevFragmentsIds = fragments.keys.toSet
    val prevVirusesIds = viruses.keys.toSet
    val prevFoodsIds = foods.keys.toSet
    val prevPlayerIds = players.keys.toSet

    val curEjectionIds = _ejections.map(t => t.pId).toSet
    val curFragmentsIds = _fragments.keys.toSet
    val curVirusesIds = _viruses.keys.toSet
    val curFoodsIds = _foods.keys.toSet
    val curPlayerIds = _players.keys.toSet

    isWorldChanged = (
      prevEjectionIds != curEjectionIds ||
      prevFragmentsIds != curFragmentsIds ||
      prevVirusesIds != curVirusesIds ||
      prevFoodsIds != curFoodsIds ||
      prevPlayerIds != curPlayerIds
      )

    ejections = _ejections
    fragments = _fragments
    viruses = _viruses
    foods = _foods
    ejections = _ejections
    players = _players
    tick = _tick

    chart.updateChart(this)
  }

  def isWorldUpdated() : Boolean = {
    isWorldChanged
  }

  def updateFragments(_fragments : Map[String, Fragment]): Unit = {
    fragments = _fragments
  }

  def   updatePlayers(_players : Map[String, Player]): Unit = {
    players = _players
  }

  def updateFoods(_foods : Map[Point, Food]): Unit = {
    foods = _foods
  }

  def getPlayer(id : String) : Option[Player] = {
    players.get(id)
  }

  var maxRadiusFragment : Option[Fragment] = None
  var maxRadiusFragmentCount = 0

  def getMaxRadius(): Double = {
    if (maxRadiusFragmentCount != fragments.size && fragments.size > 0) {
      maxRadiusFragmentCount = fragments.size
      maxRadiusFragment = Some(fragments.values.map(f => (f.circle.r, f)).maxBy(_._1)._2)
    }

    if (maxRadiusFragment.isDefined) {
      maxRadiusFragment.get.circle.r
    } else {
      0.0
    }
  }

  def getMinDistanceFragment(point : Point): Option[Fragment] = {

    if (fragments.size == 0) {
      return None
    }

    Some(fragments.values.map(f => (f.circle.point.distance(point), f)).minBy(_._1)._2)
  }
}