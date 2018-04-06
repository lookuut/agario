package com.agario

import com.agario.actions.ActionManager
import com.agario.models._
import com.agario.utils.Point

import scala.util.Random


class Strategy(val world : World) {

  val actionManager = new ActionManager(world)
  val playerHistory = new PlayerHistory(world)

  def tick(fragments : Array[Entity],
           entities : Array[Entity],
            tick : Int)
  : Response = {

    val players = entities.
      filter(t => t.objectType.getOrElse("") == "P").
      map{t =>
        val lastPos = playerHistory.getPlayerHistory(tick - 1, t.id.get)
        val player = t.player(lastPos)
        (player.id, player)
      }.
      toMap

    world.updateWorld(
      fragments.map{
        case t =>
          val fragment = t.fragment(world)
          (fragment.id, fragment)
      }.toMap
      ,
      entities.
        filter(t => t.objectType.getOrElse("") == "V").
        map{t =>
          val virus = t.virus(world)
          (virus.id, virus)
        }.
        toMap
      ,
      entities.
        filter(t => t.objectType.getOrElse("") == "F").
        map(t => (t.point, t.food(world))).
        toMap
      ,
      entities.
        filter(t => t.objectType.getOrElse("") == "E").
        map(t => t.ejection)
      , players, tick)

    playerHistory.addHistory(tick, players)

    actionManager.run(world)
  }


  def findVirus(point: Point, viruses : Iterable[Virus], maxDistToVirus : Double) : Option[Virus] = {
    val virusesWithDistance = viruses.toStream
                                  .map(f => (f, f.circle.point.distance(point)))
                                  .filter{case ((f : Virus, d : Double)) => (d < maxDistToVirus)}

    if (virusesWithDistance.size > 0) Some(virusesWithDistance.minBy(_._2)._1) else None
  }
}

object Strategy {
  val rand = new Random(12345676)
}

