package com.agario

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

class SmartLogger(private val mode : Boolean = false) {
  val logger = Logger(LoggerFactory.getLogger(this.getClass))


  def info(info : String) = if (mode) logger.info(info)
  def error(error : String) = if (mode) logger.error(error)
  def debug(debug : String) = if (mode) logger.debug(debug)
}
