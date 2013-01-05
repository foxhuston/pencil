package com.aethereus

import akka.actor._

class Npc extends Actor {
	var currentRoomName = "Start" //ToDo: Figure out better way to initialize this
	var nick = ""
	var hp = 10
	var mp = 10
	
	var strength = 10
	var dexterity = 10
	var intelligence = 10
	
	var armor = 10
	var speed = 10
	
  def receive = {
    case Attack =>
      
  }
}