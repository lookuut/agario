package com.agario

import com.agario.actions.ActionManager
import com.agario.models._
import com.agario.utils.Point

import scala.util.Random


class Strategy(val world : World) {

  val actionManager = new ActionManager(world)

  def tick(fragments : Array[Entity],
           entities : Array[Entity],
            tick : Int)
  : Response = {

    world.updateWorld(fragments, entities, tick)
    actionManager.run(world)
  }


  def findVirus(point: Point, viruses : Iterable[Virus], maxDistToVirus : Double) : Option[Virus] = {
    val virusesWithDistance = viruses.toStream
                                  .map(f => (f, f.posCircle.point.distance(point)))
                                  .filter{case ((f : Virus, d : Double)) => (d < maxDistToVirus)}

    if (virusesWithDistance.size > 0) Some(virusesWithDistance.minBy(_._2)._1) else None
  }
}

object Strategy {
  val rand = new Random(12345676)
}

