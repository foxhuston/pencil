package com.aethereus

import akka.actor._
import akka.util.ByteString
import akka.actor.IO._
import akka.pattern.ask

class Player(server: ServerHandle, room: ActorRef) extends Actor {
	var nick = ""
	var hp = 10
	var mp = 10
	var description = ""
	var race = ""
	
	val socket = server.accept()
	var Room = room
	
	room ! Description
  
	def receive = {
	  case IO.Read(readHandle, byteString) =>
	    readHandle.asSocket write byteString.compact
	  case IO.Close(s) =>
	    context.stop(self)
	  case Write(s) =>
	    socket write ByteString(s + "\r\n")
	}
}