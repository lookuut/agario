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

  def intersect(line : Line) : Option[Point] = {
    val divide = (yc * line.xc - xc * line.yc)
    if (divide == 0) {
      return None
    }

    val y = (xc * line.c - c * line.xc) / divide
    val x = if (xc != 0)  -(yc * y + c) / xc
            else if (line.xc != 0) -(line.yc * y + line.c) / line.xc
            else throw new Exception("Error in intersect lines")

    Some(new Point(x, y))
  }

  override def toString = f"$xc $yc $c"
}


object Line {

  def tangentCircle(tangentPoint1: Point, tangent1 : Point,tangentPoint2: Point, tangent2 : Point) : Option[Circle] = {
    val angle = (math.Pi - tangent1.angle(tangent2)) / 2
    val line1 = twoPoint(tangentPoint1, tangentPoint1 + tangent1)
    val line2 = twoPoint(tangentPoint2, tangentPoint2 + tangent2)

    val intersectPoint = line1.intersect(line2)

    if (intersectPoint.isEmpty) {
      return None
    }

    val radius = (tangentPoint1 - intersectPoint.get).length() * math.tan(angle)
    val center = (tangent1.turn(math.Pi / 2, -1).normalize() * radius + tangentPoint1)
    Some(new Circle(center, radius))
  }

  def twoPoint(p1 : Point, p2 : Point): Line = {
    if (p2.x == p1.x && p2.y != p1.y) {//x const
      return new Line(1, 0, -p1.x)
    }

    if (p2.x != p1.x && p2.y == p1.y) {//y const
      return new Line(0, 1, -p2.y)
    }

    val a = (p2.y - p1.y) / (p2.x - p1.x)
    val c =  p1.y - p1.x * (p2.y - p1.y) / (p2.x - p1.x)
    new Line(a, -1, c)
  }

  def intersect(fLineStart : Point, fLineEnd : Point, sLineStart : Point, sLineEnd : Point): Option[Point] = {
    val dir1 = fLineEnd - fLineStart
    val dir2 = sLineEnd - sLineStart

    //считаем уравнения прямых проходящих через отрезки
    val a1 = -dir1.y
    val b1 = +dir1.x
    val d1 = -(a1 * fLineStart.x + b1 * fLineStart.y)

    val a2 = -dir2.y
    val b2 = +dir2.x
    val d2 = -(a2 * sLineStart.x + b2 * sLineStart.y)

    //подставляем концы отрезков, для выяснения в каких полуплоскотях они
    val seg1Line2Start = a2 * fLineStart.x + b2 * fLineStart.y + d2
    val seg1Line2End = a2 * fLineEnd.x + b2 * fLineEnd.y + d2

    val seg2Line1Start = a1 * sLineStart.x + b1 * sLineStart.y + d1
    val seg2Line1End = a1 * sLineEnd.x + b1 * sLineEnd.y + d1

    //если концы одного отрезка имеют один знак, значит он в одной полуплоскости и пересечения нет.
    if (seg1Line2Start * seg1Line2End > 0 || seg2Line1Start * seg2Line1End > 0) return None

    if (seg1Line2Start - seg1Line2End == 0) {//пересечений бесконечное кол-во
      Some(new Point(Double.PositiveInfinity, Double.PositiveInfinity))
    } else {
      val u = seg1Line2Start / (seg1Line2Start - seg1Line2End)

      Some(fLineStart + (dir1 * u))
    }

  }

}