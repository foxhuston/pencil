package com.aethereus

import akka.actor._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

abstract class Npc(var room: ActorRef) extends Actor with Inventory with Fightable {
	var currentRoomName = "Start" //ToDo: Figure out better way to initialize this
	var name: String
	
	var attacking: ActorRef = null
	var attackingNick: String = ""
	
	val npcSpecificActions: PartialFunction[Any, Unit] = {
	  case _ => Unit
	}
	
	room ! Enter(name)
	
	val handleAttack: PartialFunction[Any, Unit] = {
      case Attack(who, what, damage) =>
	      if(what.toLowerCase == name.toLowerCase) {
	    	  val (message, alive) = processAttack(damage)
	    	  if(!alive)
	    	  {
	    	    attacking ! Died
	    	    room ! Leave
	    	    context.stop(self);
	    	    room ! Say(name, "Died")
	    	  }
	    	  Console.println(message)
	      }
	}
	
  def receive = handleAttack orElse npcSpecificActions
}

class Gremlin(sroom: ActorRef) extends Npc(sroom) {
  var name = "Gremlin"
  room ! Say(name, "Um. Hey, man.")
  room ! RandomRoomInhabitant
  
  context.system.scheduler.schedule(1 second, 1 second, self, "tick")
  
  override val npcSpecificActions: PartialFunction[Any, Unit] = {
    case RandomRoomInhabitantResponse(actor) =>
      attacking = actor
      attacking ! GetNick
    case GetNickResponse(nick) =>
      attackingNick = nick
    case "tick" =>
      if(attackingNick != "") {
        room ! Attack(name, attackingNick, new Damage(strength + getStrengthBonuses(), Strength, damageRoll()))
      }
      
  }
}