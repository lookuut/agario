package com.agario

import com.agario.actions.ActionManager
import com.agario.models._

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