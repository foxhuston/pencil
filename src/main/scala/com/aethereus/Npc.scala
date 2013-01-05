package com.aethereus

import akka.actor._

abstract class Npc(var room: ActorRef) extends Actor with Inventory with Fightable {
	var currentRoomName = "Start" //ToDo: Figure out better way to initialize this
	var name: String
	
	room ! Enter
	
  def receive = {
    case Attack(who, what, how, roll, damageRoll) =>
      Console.println("Saw attack: " + (who, what))
      if(what.toLowerCase == name.toLowerCase) {
    	  val (message, alive) = processAttack(how, roll, damageRoll)
    	  if(!alive)
    	  {
    	    room ! Leave
    	    context.stop(self);
    	    room ! Say(name, "Died")
    	  }
    	  Console.println(message)
      }
  }
}

class Gremlin(sroom: ActorRef) extends Npc(sroom) {
  var name = "Gremlin"
  room ! Say(name, "Um. Hey, man.")
}