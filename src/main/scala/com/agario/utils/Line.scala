package com.agario.utils


class Line(val xc : Double, val yc : Double, val c : Double) {

  /**
    *
    * @param point
    * @return 1 if up of line 0 on line , -1 down of line
    */
  def pointPos (point : Point): Int = {
    val side = point.x * xc + point.y * yc + c
    if (side > 0) 1 else if (side < 0) -1 else 0
  }

  override def toString = f"$xc $yc $c"


}