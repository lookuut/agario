package com.agario.actions

import com.agario.BaseSpec
import com.agario.models.Fragment
import com.agario.utils.{Circle, Point}

class ActionUniteSpec extends BaseSpec {

  "Test ttfFragments" in {
    val fragments = Map(
      "1" -> new Fragment("1", new Circle(new Point(100, 100), 40 / math.sqrt(40)), 40.0, new Point(0, 0), 10),
      "2" -> new Fragment("2", new Circle(new Point(120, 120), 40 / math.sqrt(40)), 40.0, new Point(0, 0), 10)
    )
    true
    //ActionUnite.ttfFragments(fragments) mustEqual Map(10 -> (new Point(110, 110), 80))
  }
}
