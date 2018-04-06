package com.agario.models

import com.rojoma.json.v3.util.AutomaticJsonCodecBuilder

class Response(val X : Double, val Y : Double,
               val Split : Boolean = false,
               val Eject : Boolean = false) {
  
  override def toString() = f"""$X $Y $Split $Eject"""
}

object Response {
  implicit val jCodec = AutomaticJsonCodecBuilder[Response]
}


