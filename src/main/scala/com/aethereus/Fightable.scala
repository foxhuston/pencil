package com.aethereus

abstract trait Fightable {
	var exp = 0
	var level = 1
	  
	var hp = 10
	var mp = 10
	
	var strength = 12
	var dexterity = 12
	var intelligence = 12
	
	var armor = 10
	var speed = 10
	
	def getStrengthBonuses(): Int
	def getDexterityBonuses(): Int
	def getIntelligenceBonuses(): Int
	def getArmorBonuses(): Int
	def getSpeedBonuses(): Int
	
	def processAttack(how: String, roll: Int, attackRoll: Int): (String, Boolean) = {
	  how match {
	        case "str" =>
	        	if(roll > strength + getStrengthBonuses()) {
	        	  hp -= attackRoll
	        	  if(hp < 0) {
	        	    return ("You died", false)
	        	  }
	        	  return ("You were hit for " + attackRoll + " hp!", true)
	        	}
	        	return ("The attack missed!", true)
	      }
	}
}