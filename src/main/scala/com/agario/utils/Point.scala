package com.agario.utils

class Point(val x: Double, val y : Double) {

  def cut(min : Point, max : Point): Point =  {

    new Point(math.min(max.x, math.max(min.x, x)), math.min(max.y, math.max(min.y, y)))
  }

  def toInt() = new Point(x.floor.toInt, y.floor.toInt)

  def squareDistance(that : Point) = (that.x - x) * (that.x - x) + (that.y - y) * (that.y - y)

  def distance(that: Point) = math.sqrt(squareDistance(that))

  def normalize() = if (length() == 0) Point.zero else new Point(x / length, y / length)

  def length() = Math.sqrt(x * x + y * y)

  def invert () = this * -1

  def angle(that: Point): Double = {

    val delta = (that.x * x + that.y * y) / Math.sqrt((x * x + y * y) * (that.x * that.x + that.y * that.y))

    if (delta > 1.0) return 0.0
    if (delta < -1.0) return Math.PI

    return Math.acos(delta)
  }

  def angleAgainstClockWay(that : Point) = math.atan2(this.cross(that), this.dot(that))

  /**
    * @param p
    * @return if 1 then p on left side or top else lower side
    */
  def side (p : Point) : Double = {
    val turnAngle = (this - p)

    if (math.atan2(turnAngle.y, turnAngle.x) < 0) 1
    else -1

  }

  def mirrorVector(normal : Point) : Point = {
    (this - normal * (normal.dot(this)) * (1.0 / normal.dot(normal)) * 2).invert()
  }

  def dot(vec : Point) : Double = {
    vec.x * x + vec.y * y
  }

  def normal() : Point = {
    this.turn(math.Pi / 2)
  }

  def cross(p : Point): Double = x * p.y - y * p.x


  /**
    * Turn vector , way == 1 is against clock way
    * @param angle
    * @return
    */
  def turn (angle : Double, way : Int = 1) : Point = {
    new Point (x * math.cos(angle) - way * y * math.sin(angle), way * x * math.sin(angle) + y * math.cos(angle))
  }

  def - (that: Point) = new Point(x - that.x, y - that.y)
  def + (that: Point) = new Point(x + that.x, y + that.y)
  def * (f : Double) = new Point(x * f, y * f)
  def * (that : Point) = new Point(x * that.x, y * that.y)

  override def toString = f"$x $y"

  override def equals(that: Any): Boolean =
    that match {
      case that: Point => that.x == x && that.y == y
      case _ => false
    }

  override def hashCode() : Int = {
    var bits: Long = 7L
    bits = 31L * bits + java.lang.Double.doubleToRawLongBits(x)
    bits = 31L * bits + java.lang.Double.doubleToRawLongBits(y)
    (bits ^ (bits >> 32)).toInt
  }

  implicit def doubleToPoint(x: Double) = new Point(x, x)
  implicit def intToPoint(x: Int) = new Point(x, x)
}


object  Point {
  val zero = new Point(0,0)

  def line(p1 : Point, p2 : Point): Line = {
    val a = (p2.y - p1.y) / (p2.x - p1.x)
    val c =  p1.y - p1.x * (p2.y - p1.y) / (p2.x - p1.x)
    new Line(a, -1, c)
  }
}
