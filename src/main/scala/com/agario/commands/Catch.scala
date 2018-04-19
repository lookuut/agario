package com.agario.commands

import com.agario.models.{BaseEntity, Food, Fragment, World}
import com.agario.navigation.{BaseField, Track}
import com.agario.utils.{Circle, Line, Point, Trajectory}

class Catch(fragment: Fragment, entity : BaseEntity, track : Track, startTick : Int) extends Command(fragment, entity, track, startTick) {

  var correctedTrack : Track = null
  var correctedTrackStartTick = 0
  override def run() : (Point, Point, Boolean)  = {

    if (correctedTrack != null && correctedTrack.duration() + correctedTrackStartTick > World.tick) {
      return (fragment.posCircle.point, correctedTrack.getStep(World.tick - correctedTrackStartTick).get.direction, false)
    }

    val victimPos = (entity.posCircle.point - fragment.posCircle.point)

    if (isFinished()) {
      return (fragment.posCircle.point, victimPos, false)
    }


    if (World.fragments.size < World.config.maxFragmentsCount && fragment.weight > Fragment.minWeightToSplit
      && fragment.weight / 2 > 1.2 * entity.weight
    ) {
      val angle = victimPos.angleAgainstClockWay(fragment.speed)

      if (math.abs(angle) < math.Pi / 18) {//split to catch
        return (fragment.posCircle.point, victimPos, true)
      }
    }

    if (entity.speed.length() > 0) {
      val enemyTrack = Trajectory.directionTrack(entity,entity.speed,BaseField.empty, 50)
      if (enemyTrack.duration() > 0) {
        val step = math.ceil(enemyTrack.duration().toDouble / 2).toInt
        val ePos = enemyTrack.getStep(step).get.position
        val cTrack = Trajectory.optimalTrack(fragment, new Circle(ePos, Food.radius), BaseField.empty)
        if (cTrack.duration() > 0){
          this.correctedTrack = cTrack
          this.correctedTrackStartTick = World.tick

          return (fragment.posCircle.point, correctedTrack.track.get(0).get.direction, false)
        }
      }
    }

    return (fragment.posCircle.point, victimPos, false)
  }

  override def isFinished(): Boolean = {
    World.entities.get(entity.getId()).isEmpty || !entity.canEat(fragment)
  }
}