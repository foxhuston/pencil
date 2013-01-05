package com.aethereus

abstract trait Fightable {
	var exp = 0
	var level = 1
	  
	var hp = 10
	var mp = 10
	
	var strength = 10
	var dexterity = 10
	var intelligence = 10
	
	var armor = 10
	var speed = 10
	
	def getStrengthBonuses(): Int
	def getDexterityBonuses(): Int
	def getIntelligenceBonuses(): Int
	def getArmorBonuses(): Int
	def getSpeedBonuses(): Int
	
	def processAttack(how: String, roll: Int): String = {
	  how match {
	        case "str" =>
	        	return "Test" + strength + ", " + getStrengthBonuses()
	      }
	}
}