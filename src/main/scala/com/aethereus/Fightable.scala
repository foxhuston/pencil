package com.aethereus

import scala.util.Random

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
	
	val fightableRandom = new Random()

	def hitCheck(mine: Int, them: Int) = {
	  fightableRandom.nextDouble() >= 1/(1+Math.pow(Math.E,(-.4 * (mine-them-5))))
	}
	
	def processAttack(withDamage : Damage): (String, Boolean) = {
	  withDamage.againstWhat match {
	        case Strength =>
	        	if(hitCheck(strength + getStrengthBonuses(), withDamage.attackerLevel)) {
	        	  hp -= withDamage.damageRoll
	        	  if(hp < 0) {
	        	    return ("You died", false)
	        	  }
	        	  return ("You were hit for " + withDamage.damageRoll + " hp!", true)
	        	}
	        	return ("The attack missed!", true)
	      }
	}
}