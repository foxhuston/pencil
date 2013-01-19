package com.aethereus

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._
import akka.util.ByteString
import akka.actor.IO._
import akka.pattern.ask
import util.Random
import org.neo4j.scala.RestGraphDatabaseServiceProvider
import org.neo4j.scala.Neo4jIndexProvider
import java.net.URI
import ua.t3hnar.bcrypt._
import org.neo4j.graphdb._
import org.neo4j.scala.Neo4jWrapper

class Player(server: ServerHandle, var room: ActorRef) extends Actor with Neo4jWrapper with Fightable with Inventory with RestGraphDatabaseServiceProvider with Neo4jIndexProvider {
    var currentRoomName = "Start" //ToDo: Figure out better way to initialize this
	var nick = ""
	  
	def uri = new URI("http://localhost:7474/db/data/")
	
	val playerIndex = ds.gds.index.forNodes("PlayerIndex");
	  
	val lives = 10
	
	addToInventory(WoodenSword)
	  
	var description = ""
	var race = ""
	
	val socket = server.accept()
	val roomService = context.actorFor("../RoomService")
	
	var parseState = "Get Nick"
	var nickParseState = ""
	var roomParseState = ""
	var tmpDescription = ""
	var tmpDirection = ""
	var tmpRoomName = "" 
	  
	var lastAttacked : (String, DamageType) = ("", null)
	
	var node: Node = null
	
	var combatTimer: Cancellable = null 
	
	self ! Write("[228 Your name?]")
	
	
	// Pass verbs -> room -> room objects
	val create = "(c|create)$".r
	val whoAmI = "(whoami)$".r
	val look = "(l|look|description)".r
	val say = "^('|say[ ]+)(.*)".r
	val get = "^get(.*)".r
    val give = "^drop".r
    val describe = "^describe[ ]+(.*)$".r
    val drop = ""
    val take = ""
    val put = ""
    val tell = ""
    val alias = ""
    val unequip = "(lower|unequip|remove)(.*)".r
    val who = "^who$".r
    val open = ""
    val close = ""
    val quit = "^quit$".r
    val stats = "(score|stats)".r
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
	    case stats(_) =>
	      printStats
	    case create(_) =>
	      self ! Write("What direction are you traveling?")
	      parseState = "RoomParser"
	    case look(_) =>
	      room ! EnterMessage
	    case whoAmI(_) =>
	      self ! Write("You are " + nick)
	    case testAttack(what) =>
	      lastAttacked = (what, Strength)
	      combatTimer = context.system.scheduler.schedule(0 second, (10/speed) second, self, "combatTick")
	    case equip(_, what) =>
	      equipItem(what)
	    case matchInventory(_) =>
	      self ! Write(printInventory())
	    case describe(newDescription) =>
	      room ! SetDescription(newDescription)
	    case quit() =>
	      room ! Leave
	      self ! Write("Hey thanks for playing.")
	      socket.close()
	    case who() =>
	      context.actorSelection("../*") ! GetNick
	}
	
	val parseDebugBehavior: PartialFunction[String, Unit] = {
	  case debugSpawn(_, "gremlin") =>
	    context.actorOf(Props(new Gremlin(room)))
	}
	
	def parseTravel: PartialFunction[String, Unit] = {
  	    case exit =>
	   	  room ! LeaveBy(nick, exit)
	}
    
	def getAttributesFromDatabase() = {
	  strength = node("strength").getOrElse("10").toInt
	  speed = node("speed").getOrElse("10").toInt
	}
	
    def parseGetNick(input: String) = {
      if(nickParseState == "")
      {
	      nick = input
	      
	      withTx {
	        implicit neo =>
		      val res = playerIndex.get("nick", nick).getSingle();
		      if(res != null)
		      {
		        self ! Write("Please enter your password")
		        node = res;
		        getAttributesFromDatabase()
		      }
		      else
		      {
		        self ! Write("You're new here!\r\nPlease enter a password for this account.")
		      }
	      }
	      
	      nickParseState = "password"
      }
      else if (nickParseState == "password")
      {
        withTx {
          implicit neo =>
	        if(node != null)
	        {
	          if(input.isBcrypted(node.getProperty("password").toString()))
	          {
	            self ! Write("Successfully logged in!")
    	        parseState = ""
		        nickParseState = ""
			    room ! Enter(nick)
	          }
	          else
	          {
	            self ! Write("Please try again.")
	          }
	        }
	        else
	        {
	          self ! Write("Character created!")
	          node = createNode
	          node("type") = "player"
	          node("nick") = nick
	          node("password") = input.bcrypt
	          playerIndex.add(node, "nick",	nick)
	          parseState = ""
		      nickParseState = ""
			  room ! Enter(nick)
	        }
	      }
      }
    }
	
    val roomIndex = ds.gds.index.forNodes("RoomIndex");
    
	def roomParser(input: String) = {
	  roomParseState match {
	    case "Description" =>
	      if(input == ".") {
	    	room ! SetDescription(tmpDescription)
	    	room ! Enter(nick)
	    	parseState = ""
	    	roomParseState = ""
	    	tmpDescription = ""
	      }
	      else
	      {
	    	tmpDescription += input + "\r\n"
	      }
	    case "Name" =>
	      withTx {
	        implicit neo => 
	            val replacedInput = input.replace(' ', '_')
	        	val x = roomIndex.get("name", replacedInput).getSingle()
	        	if(x == null) {
				    roomService ! NewRoom(replacedInput)
				    tmpRoomName = replacedInput
				    self ! Write("Describe what you see. End your description with a '.' on a line by itself.")
				    roomParseState = "Description"
	        	}
	        	else
	        	{
	        	  self ! Write("Sorry, that room name is taken. Please enter another name.")
	        	}
	      }
	    case _ =>
	      tmpDirection = input
	      self ! Write("What's the room's name?")
	      roomParseState = "Name"
	  }
	}
  
	def printStats = {
	  val str = s"Stats for $nick\r\n" +
	    s"Health: $hp\r\n" +
	    s"Magic: $mp\r\n\r\n" +
	    s"Strength: $strength\r\n" +
	    s"Dexterity: $dexterity\r\n" +
	    s"Intelligence: $intelligence\r\n\r\n" +
	    s"Armor: $armor\r\n" +
	    s"Speed: $speed\r\n"
	  
	  self ! Write(str)
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
	    socket write ByteString(Color.parse(s) + "\r\n> ")
	  case JoinMessage(player) =>
	    player ! SendMeAJoinMessage
	  case SendMeAJoinMessage =>
	    context.sender ! Write(nick + " joined the room.")
	  case LeaveMessage(player) =>
	    player ! SendMeALeaveMessage
	  case SendMeALeaveMessage =>
	    context.sender ! Write(nick + " left the room.")
	  case LeaveOk(r) =>
	    if(combatTimer != null) combatTimer.cancel()
	    
	    val (d, roomName) = r
	    currentRoomName = roomName
	    room = context.actorFor("../RoomService/" + roomName)
	    room ! Enter(nick)
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
	  case "combatTick" =>
	    var (what, how) = lastAttacked
	    room ! Attack(nick, what, new Damage(strength + getStrengthBonuses, how, damageRoll()))
	  case Died =>
	    combatTimer.cancel()
	  case Attack(who, what, damage) =>
	    if(nick == what) {
          self ! Write(who + " is attacking!")
	      val (message, alive) = processAttack(damage)
	      self ! Write(message)
	      if(!alive)
	      {
	        hp = 10
	        room ! Leave
	        room = context.actorFor("../RoomService/Start")
	        room ! Enter(nick)
	      }
	    } else {
	      self ! Write(who + " is attacking " + what + "!")
	    }
	    
	  case GetNick =>
	    context.sender ! GetNickResponse(nick)
	   
	  case GetNickResponse(nick) =>
	    socket write ByteString(s"$nick\r\n")
	    
	}
}