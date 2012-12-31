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
	
	var strength = 10
	var dexterity = 10
	var intelligence = 10
	
	var armor = 10
	var speed = 10
	
	var description = ""
	var race = ""
	
	val socket = server.accept()
	val roomService = context.actorFor("../RoomService")
	
	var parseState = "Get Nick"
	var roomParseState = ""
	var tmpDescription = ""
	var tmpDirection = ""
	
	self ! Write("Your name?")
	
	val create = "(c|create)$".r
	val whoAmI = "(w|whoami)$".r
	val look = "(l|d|look|description)".r
	val say = "^'(.*)".r
	val shout = "^\"".r
	
	def parseBehavior(input: String) = {
	  input match {
	    case say(input) =>
	      room ! Say(nick, input)
	    case create() =>
	      self ! Write("What direction are you traveling?")
	      parseState = "RoomParser"
	    case look() =>
	      room ! EnterMessage
	    case whoAmI() =>
	      self ! Write("You are " + nick)
	    case _ =>
	   	  room ! LeaveBy(input)   
	  }
	}
    
    def parseGetNick(input: String) = {
      nick = input
      room ! Enter
      parseState = ""
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
	    else if(parseState == "Get Nick")
	    	parseGetNick(input)
	    else
	      parseBehavior(input)

	  case IO.Closed(s, cause) =>
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
	    val (d, roomName) = r
	    Console.println("Room's Name: " + roomName)
	    currentRoomName = roomName
	    room = context.actorFor("../RoomService/" + roomName)
	    room ! Enter
	  case LeaveFail =>
	    self ! Write("You can't go that way")
	    
	  case ReportHit(who, damage) =>
	    self ! Write("You hit " + who + " for " + damage + " damage")
	  case ReportMiss(who) =>
	    self ! Write("You missed " + who)
	    
	  case Attack(who, what, roll) =>
	    self ! Write(who + " is attacking!")
	    
	}
}