package com.aethereus

import akka.actor._
import akka.util.ByteString
import akka.actor.IO._
import akka.pattern.ask

class Player(server: ServerHandle, var room: ActorRef) extends Actor {
    var currentRoomName = "Start" //ToDo: Figure out better way to initialize this
	var nick = ""
	var hp = 10
	var mp = 10
	var description = ""
	var race = ""
	
	val socket = server.accept()
	val roomService = context.actorFor("../RoomService")
	
	var parseState = ""
	var roomParseState = ""
	var tmpDescription = ""
	var tmpDirection = ""
	
	room ! Enter
	
	def parseBehavior(input: String) = {
      Console.println("Parsing behavior")
	  input match {
	    case "create" =>
	      self ! Write("What direction are you traveling?")
	      parseState = "RoomParser"
	    case "d" =>
	      room ! EnterMessage
	    case _ =>
	   	  room ! LeaveBy(input)   
	  }
	}
	
	def roomParser(input: String) = {
	  roomParseState match {
	    case "Description" =>
	      if(input == ".") {
	    	room ! SetDescription(tmpDescription)
	    	room ! Enter
	    	parseState = ""
	    	roomParseState = ""
	    	tmpDescription = ""
	    	tmpDirection = ""
	      }
	      else
	      {
	    	tmpDescription += input + "\r\n"
	      }
	    case "Name" =>
	      roomService ! NewRoom(input)
	      room ! AddExit (tmpDirection, input)
	      val newRoom = context.actorFor("../RoomService/" + input)
	      room ! Leave
	      newRoom ! AddExit("back", currentRoomName)
	      room = newRoom
	      
	      self ! Write("Describe what you see. End your description with a '.' on a line by itself.")
	      roomParseState = "Description"
	    case _ =>
	      tmpDirection = input
	      self ! Write("What's the room's name?")
	      roomParseState = "Name"
	  }
	}
  
	def receive = {
	  case IO.Read(readHandle, byteString) =>
	    val input = byteString.utf8String.trim()
	    if(parseState == "RoomParser")
	    	roomParser(input)
	    else
	      parseBehavior(input)

	  case IO.Closed(s, cause) =>
	    Console.println(nick + " quit")
	    room ! Leave
	    context.stop(self)
	  case Write(s) =>
	    socket write ByteString(s + "\r\n> ")
	  case JoinMessage(player) =>
	    player ! SendMeAJoinMessage
	  case SendMeAJoinMessage =>
	    context.sender ! Write(nick + " joined the room.")
	  case LeaveMessage(player) =>
	    player ! SendMeALeaveMessage
	  case SendMeALeaveMessage =>
	    context.sender ! Write(nick + " left the room.")
	  case LeaveOk(r) =>
	    Console.println(r)
	    val (d, roomName) = r
	    Console.println("Room's Name: " + roomName)
	    currentRoomName = roomName
	    room = context.actorFor("../RoomService/" + roomName)
	    room ! Enter
	  case LeaveFail =>
	    self ! Write("You can't go that way")
	}
}