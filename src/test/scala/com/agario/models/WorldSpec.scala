package com.agario.models

import com.agario.BaseSpec
import com.agario.world
import com.agario.utils.Circle

class WorldSpec extends BaseSpec {
  sequential
  "World spec" should {
    "World update spec" in new world {

      val fEntities = Array(
        new Entity(Some("1"), 100, 100, None, Some(40), Some(Fragment.radiusByWeight(40)), Some(0), Some(0), None, None),
        new Entity(Some("2"), 200, 200, None, Some(40), Some(Fragment.radiusByWeight(40)), Some(0), Some(0), None, Some(400))
      )

      val eEntitiesTick0 = Array(
        new Entity(Some("1"), 100, 100, None, Some(40), Some(Fragment.radiusByWeight(40)), Some(0), Some(0), None, None),
        new Entity(Some("2"), 200, 200, None, Some(40), Some(Fragment.radiusByWeight(40)), Some(0), Some(0), None, Some(400)),
        new Entity(Some("1"), 300, 300, Some(BaseEntity.food), Some(World.config.foodWeight), Some(Food.radius), Some(0), Some(0), None, None),
        new Entity(Some("2"), 400, 400, Some(BaseEntity.food), Some(World.config.foodWeight), Some(Food.radius), Some(0), Some(0), None, None),
        new Entity(Some("1"), 300, 300, Some(BaseEntity.virus), Some(40), Some(World.config.virusRadius), Some(0), Some(0), None, None),
        new Entity(Some("2"), 400, 400, Some(BaseEntity.virus), Some(40), Some(World.config.virusRadius), Some(0), Some(0), None, None),
        new Entity(Some("1"), 500, 500, Some(BaseEntity.player), Some(40), Some(Fragment.radiusByWeight(40)), None, None, None, None),
        new Entity(Some("2"), 600, 600, Some(BaseEntity.player), Some(40), Some(Fragment.radiusByWeight(40)), None, None, None, None),
        new Entity(Some("1"), 700, 700, Some(BaseEntity.ejection), Some(Ejection.weight), Some(Ejection.radius), None, None, Some(10), None),
        new Entity(Some("2"), 800, 800, Some(BaseEntity.ejection), Some(Ejection.weight), Some(Ejection.radius), None, None, Some(20), None)
      )

      val eEntitiesTick1 = Array(
        new Entity(Some("1"), 100, 100, None, Some(40), Some(Fragment.radiusByWeight(40)), Some(0), Some(0), None, None),
        new Entity(Some("2"), 200, 200, None, Some(40), Some(Fragment.radiusByWeight(40)), Some(0), Some(0), None, Some(400)),
        new Entity(Some("1"), 300, 300, Some(BaseEntity.food), Some(World.config.foodWeight), Some(Food.radius), Some(0), Some(0), None, None),
        new Entity(Some("2"), 400, 400, Some(BaseEntity.food), Some(World.config.foodWeight), Some(Food.radius), Some(0), Some(0), None, None),
        new Entity(Some("1"), 300, 300, Some(BaseEntity.virus), Some(40), Some(World.config.virusRadius), Some(0), Some(0), None, None),
        new Entity(Some("2"), 400, 400, Some(BaseEntity.virus), Some(40), Some(World.config.virusRadius), Some(0), Some(0), None, None),
        new Entity(Some("1"), 505, 505, Some(BaseEntity.player), Some(40), Some(Fragment.radiusByWeight(40)), Some(5), Some(5), None, None),
        new Entity(Some("2"), 605, 605, Some(BaseEntity.player), Some(40), Some(Fragment.radiusByWeight(40)), Some(5), Some(5), None, None),
        new Entity(Some("1"), 706, 706, Some(BaseEntity.ejection), Some(Ejection.weight), Some(Ejection.radius), Some(6), Some(6), Some(10), None),
        new Entity(Some("2"), 806, 806, Some(BaseEntity.ejection), Some(Ejection.weight), Some(Ejection.radius), Some(6), Some(6), Some(20), None)
      )
      World.updateWorld(fEntities, eEntitiesTick0, 0)

      World.updateWorld(fEntities, eEntitiesTick1, 1)

      World.getEntities(BaseEntity.fragment).size == 2 mustEqual true

      for(i <- 0 until eEntitiesTick1.size) {
        World.getEntity(eEntitiesTick0(i).getId()).isDefined mustEqual true
        World.getEntity(eEntitiesTick0(i).getId()).get.getId mustEqual eEntitiesTick1(i).getId
        World.getEntity(eEntitiesTick0(i).getId()).get.posCircle mustEqual new Circle(eEntitiesTick1(i).point, eEntitiesTick1(i).r.get)
        World.getEntity(eEntitiesTick0(i).getId()).get.speed mustEqual eEntitiesTick1(i).speed
        World.getEntity(eEntitiesTick0(i).getId()).get.weight mustEqual eEntitiesTick1(i).weight.get
      }

      World.getEntity(eEntitiesTick0(0).getId()).get.asInstanceOf[Fragment].ttf mustEqual eEntitiesTick1(0).ttf.getOrElse(0)
      World.getEntity(eEntitiesTick0(1).getId()).get.asInstanceOf[Fragment].ttf mustEqual eEntitiesTick1(1).ttf.getOrElse(0)

      World.getEntity(eEntitiesTick0(8).getId()).get.asInstanceOf[Ejection].pId mustEqual eEntitiesTick1(8).pId.getOrElse(-1)
      World.getEntity(eEntitiesTick0(9).getId()).get.asInstanceOf[Ejection].pId mustEqual eEntitiesTick1(9).pId.getOrElse(-1)
    }
  }
}
