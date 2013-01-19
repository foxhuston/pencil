package com.aethereus

class DamageType
case object Strength extends DamageType

class Damage(val attackerLevel: Int, val againstWhat : DamageType, val damageRoll : Int) {
	
}

class AttackRoll()
{

}