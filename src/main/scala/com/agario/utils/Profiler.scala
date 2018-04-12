package com.agario.utils

import System.currentTimeMillis

object Profiler {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()

    println("Elapsed time: " + (t1 - t0)/1000000 + "ms")
    result
  }

  def profile[R](code: => R, t: Long = currentTimeMillis) = (code, currentTimeMillis - t)
}
