package com.agario.utils

import scala.collection.mutable

case class DBScanPoint (var clusterId : Int, val point : Point)

/**
  * @ Naive realization of DBSCAN
  *
  * @param distance
  * @param minPoints
  */
class DBSCAN (val distance : Double, val minPoints : Double) {

  private val statusMap = mutable.Map[DBScanPoint, Int]()

  def clusterization(points: Iterable[DBScanPoint]) : Iterable[DBScanPoint] = {
    val indexedPoints = points.map(p => (spatialIndex(p) , p)).
                          groupBy(_._1).
                          map{case (p , values) => (p, values.map(_._2).toList)}

    var clusterId = 1
    points.foreach(point => {
      if (!statusMap.contains(point)) {
        val neighbors = nearPoints(point, indexedPoints)
        if (neighbors.length < minPoints) {
          statusMap(point) = NOISE
        } else {
          clusterId = expand(point, neighbors, clusterId, indexedPoints)
        }
      }
    })
    points
  }

  private val CLASSIFIED = 1
  private val UNCLASSIFIED = 2
  private val NOISE = 3

  private def expand(point : DBScanPoint,
                     neighbors: Iterable[DBScanPoint],
                     clusterID: Int,
                     indexedPoints : Map[Point, Iterable[DBScanPoint]]
                    ): Int = {

    point.clusterId = clusterID
    statusMap(point) = CLASSIFIED
    val queue = new mutable.Queue[DBScanPoint]
    queue ++= neighbors
    while (queue.nonEmpty) {
      val neighbor = queue.dequeue
      statusMap.getOrElse(neighbor, UNCLASSIFIED) match {
        case UNCLASSIFIED => {
          neighbor.clusterId = clusterID
          statusMap(neighbor) = CLASSIFIED
          val neighborNeighbors = nearPoints(neighbor, indexedPoints)
          if (neighborNeighbors.length >= minPoints) {
            queue ++= neighborNeighbors
          }
        }
        case NOISE => {
          neighbor.clusterId = clusterID
          statusMap(neighbor) = CLASSIFIED
        }
        case _ =>
      }
    }
    clusterID + 1
  }




  def spatialIndex(p : DBScanPoint) = new Point((p.point.x / distance).floor.toInt, (p.point.y / distance).floor.toInt)

  def nearPoints(point : DBScanPoint, indexedPoints : Map[Point, Iterable[DBScanPoint]]): List[DBScanPoint] = {
    val pIndex = spatialIndex(point)

    (pIndex.x.toInt - 1 to pIndex.x.toInt + 1).flatMap(i =>
      (pIndex.y.toInt - 1 to pIndex.y.toInt + 1).flatMap(j =>
        indexedPoints.get(new Point(i, j)).getOrElse(List()).filter(p => point != p).toList
      )
    ).toList
  }
}
