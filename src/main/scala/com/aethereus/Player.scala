package com.aethereus

import akka.actor._
import akka.actor.IO._

class Player(server: ServerHandle) extends Actor {
	val socket = server.accept()
  
	def receive = {
	  case IO.Read(readHandle, byteString) =>
	    readHandle.asSocket write byteString.compact
	  case IO.Close(s) =>
	    context.stop(self)
	}
}