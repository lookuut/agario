package com.agario.models

import com.agario.utils.Circle

class Virus (val id : String,
             val circle : Circle,
             val weight : Double
               ) {

  override def equals(that: Any): Boolean =
    that match {
      case that: Food => that.circle == circle
      case _ => false
    }

  override def hashCode() : Int = {
    id.hashCode
  }
}