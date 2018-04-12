package com.agario.models

import com.rojoma.json.v3.util.AutomaticJsonCodecBuilder

class Response(val X : Double, val Y : Double,
               val Split : Boolean = false,
               val Eject : Boolean = false,
               val Debug : String = "") {

  def this(r : Response, d : String) = this(r.X, r.Y, r.Split, r.Eject, d)

  override def toString() = f"""$X $Y $Split $Eject $Debug"""
}

object Response {
  implicit val jCodec = AutomaticJsonCodecBuilder[Response]
}


