package com.agario

import java.net.{InetAddress, Socket}
import java.io._

import com.agario.models._
import com.agario.utils.Profiler
import com.rojoma.json.v3.util.JsonUtil

import scala.collection.Iterator
import scala.io.BufferedSource
import scala.util.Random

object Main {

	var logger : SmartLogger = new SmartLogger()

	val rand = new Random(123)
	
	def run() {
		val configStr = input()
		val config = JsonUtil.parseJson[Config](configStr)
		val world = new World(config.right.get)
		val strategy = new Strategy(world)

		logger.info(s"Get config file " + configStr)

		try {
			for(tick <- 0 to config.right.get.ticks) {

				val line = input()
				logger.info(s"Get line " + line)

				val parsed = JsonUtil.parseJson[Map[String, Array[Entity]]](line)

				val fragments = parsed.right.get("Mine")
				val entities = parsed.right.get("Objects")

				var response = new Response(0, 0, false, false)

				val mills = Profiler.profile{
					response = strategy.tick(
						fragments,
						entities ++ fragments,
						tick
					)
				}

				val jsonResponse = JsonUtil.renderJson(new Response(response, f"Run time $mills ms"))
				logger.info(jsonResponse)
				output(jsonResponse)
			}

		} catch {
			case e: NumberFormatException =>

			/*case e: Exception => {
				println(e.getStackTrace.toString)
				val response = new Response(0,0, false, false, e.getStackTrace.mkString("\n"))
				val jsonResponse = JsonUtil.renderJson(response)
				logger.info(jsonResponse)
				output(jsonResponse)
			}*/
		}
	}

	var netStream = false

	var out : PrintStream = null
	var in : Iterator[String] = null

	def output(outString : String) : Unit = {

		if (netStream) {
			out.println(outString)
		} else {
			println(outString)
		}
	}

	def input () : String = {
		if (netStream) {
			in.next.trim
		} else {
			scala.io.StdIn.readLine().trim
		}
	}

	def main(args : Array[String]) {
		try {
			if (args.filter(t => t == "log_enabled").size > 0) {
				logger = new SmartLogger(true)
			}

			if (args.filter(t => t == "net").size > 0) {
				netStream = true
				val socket = new Socket(InetAddress.getByName("localhost"), 1234)
				in = new BufferedSource(socket.getInputStream).getLines
				out = new PrintStream(socket.getOutputStream)
			}

			run
		} catch {
			case e: Exception => logger.error(e.getStackTrace.mkString("\n"))
		}
	}
}