package com.aethereus

import akka.actor._
import akka.util.ByteString
import akka.actor.IO._
import akka.pattern.ask

import util.Random

class Player(server: ServerHandle, var room: ActorRef) extends Actor with Fightable with Inventory {
    var currentRoomName = "Start" //ToDo: Figure out better way to initialize this
	var nick = ""
	  
	val lives = 10
	
	addToInventory(WoodenSword)
	  
	var description = ""
	var race = ""
	
	val socket = server.accept()
	val roomService = context.actorFor("../RoomService")
	
	var parseState = "Get Nick"
	var roomParseState = ""
	var tmpDescription = ""
	var tmpDirection = ""
	var tmpRoomName = "" 
	  
	var lastAttacked = ("", "")
	
	self ! Write("Your name?")
	
	
	// Pass verbs -> room -> room objects
	val create = "(c|create)$".r
	val whoAmI = "(whoami)$".r
	val look = "(l|look|description)".r
	val say = "^('|say[ ]+)(.*)".r
	val get = "^get(.*)".r
    val give = "^drop".r
    val describe = "^describe(.*)$".r
    val drop = ""
    val take = ""
    val put = ""
    val tell = ""
    val alias = ""
    val unequip = "(lower|unequip|remove)(.*)".r
    val open = ""
    val close = ""
    val quit = ""
    val stats = "(score|stats)"
	val shout = "^\"".r
	val testAttack = "^kill[ ]+(.*)$".r
	val testAttackShort = "^a$".r
	val equip = "^(equip|wear|wield)[ ]+(.*)$".r
	val matchInventory = "^(i|inventory)$".r
	val debugSpawn = "^(s|spawn)[ ]+(.*)".r
	
	val debugShutdown = "^shutdown$".r
	
    def parseBehavior(input: String) = 
      (parseBehaviorA orElse parseDebugBehavior orElse parseTravel)(input)
    
	val parseBehaviorA: PartialFunction[String, Unit] = {
	    case say(_, input) =>
	      room ! Say(nick, input)
	    case create(_) =>
	      self ! Write("What direction are you traveling?")
	      parseState = "RoomParser"
	    case look(_) =>
	      room ! EnterMessage
	    case whoAmI(_) =>
	      self ! Write("You are " + nick)
	    case testAttackShort() =>
	      val (w, h) = lastAttacked
	      room ! Attack(nick, w, h, roll(), damageRoll())
	    case testAttack(what) =>
	      lastAttacked = (what, "str")
	      room ! Attack(nick, what, "str", roll(), damageRoll())
	    case equip(_, what) =>
	      equipItem(what)
	    case matchInventory(_) =>
	      self ! Write(printInventory())
	    case describe(newDescription) =>
	      room ! SetDescription(newDescription)
	}
	
	val parseDebugBehavior: PartialFunction[String, Unit] = {
	  case debugSpawn(_, "gremlin") =>
	    context.actorOf(Props(new Gremlin(room)))
	}
	
	def parseTravel: PartialFunction[String, Unit] = {
  	    case exit =>
	   	  room ! LeaveBy(exit)
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
	      }
	      else
	      {
	    	tmpDescription += input + "\r\n"
	      }
	    case "Name" =>
	      roomService ! NewRoom(input)
	      tmpRoomName = input
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
	    socket write ByteString(s + "\r\n[" + hp + "]> ")
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
	    currentRoomName = roomName
	    room = context.actorFor("../RoomService/" + roomName)
	    room ! Enter
	  case LeaveFail =>
	    self ! Write("You can't go that way")
	  case RoomCreated(name) =>
	    //TODO: Make this not a race condition.
	    room ! AddExit (tmpDirection, tmpRoomName)
	    val newRoom = context.actorFor("../RoomService/" + tmpRoomName)
	    room ! Leave
	    newRoom ! AddExit("back", currentRoomName)
	    room = newRoom
    	tmpDirection = ""
    	tmpRoomName = ""
	  case ReportHit(who, damage) =>
	    self ! Write("You hit " + who + " for " + damage + " damage")
	  case ReportMiss(who) =>
	    self ! Write("You missed " + who)
	    
	  case Attack(who, what, how, roll, damageRoll) =>
	    if(nick == what) {
	      self ! Write(who + " is attacking! (" + roll + ", " + how + ")")
	      val (message, alive) = processAttack(how, roll, damageRoll)
	      self ! Write(message)
	      if(!alive)
	      {
	        hp = 10
	        room ! Leave
	        room = context.actorFor("../RoomService/Start")
	        room ! Enter
	      }
	    } else {
	      self ! Write(who + " is attacking " + what + "!")
	    }
	    
	  case GetNick =>
	    context.sender ! GetNickResponse(nick)
	    
	}
}