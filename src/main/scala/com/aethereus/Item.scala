package com.aethereus

abstract class Item {
	var equipped = false
	val name: String
	val description: String
	
	val hpBonus: Int
	val mpBonus: Int
	
	val strengthBonus: Int
	val dexterityBonus: Int
	val intelligenceBonus: Int
	
	val armorBonus: Int
	val speedBonus: Int
	
	val maxDamage: Int
}

object WoodenSword extends Item {
	  val name = "Wooden Sword"
    
	  val description = "A small practice sword."
      
      val hpBonus = 0
      val mpBonus = 0
      
      val strengthBonus = 1
      val dexterityBonus = 0
      val intelligenceBonus = 0
      
      val armorBonus = 0
      val speedBonus = 0
      
      val maxDamage = 2
}

trait Inventory {
  var inventory: List[(Item, Int)] = List()
  var equipped: List[Item] = List()
  
  def printInventory(): String = {
    var inventoryString = ""
      for((item, count) <- inventory) {
        if(item.equipped) inventoryString += "* "
        inventoryString += item.name + " x" + count + "\r\n"
      }
    return inventoryString
  }
  
  def addToInventory(newItem: Item) {
    inventory.find(x => x._1 == newItem)
	match {
      case Some(x) =>
        inventory = inventory diff List(x)
        inventory ::= (newItem, x._2 + 1)
      case None =>
        inventory ::= (newItem, 1) 
    }
  }
  
  def getSpeedBonuses(): Int = {
    return equipped.map(x => x.speedBonus).sum
  }
  
  def getArmorBonuses(): Int = {
    return equipped.map(x => x.armorBonus).sum
  }
  
  def getIntelligenceBonuses(): Int = {
    return equipped.map(x => x.intelligenceBonus).sum
  }
  
  def getDexterityBonuses(): Int = {
    return equipped.map(x => x.dexterityBonus).sum
  }
  
  def getStrengthBonuses(): Int = {
    return equipped.map(x => x.strengthBonus).sum
  }
  
  def equipItem(itemName: String) = {
    inventory.find(x =>
      {
        val (item, _) = x
		item.name == itemName 
      }) match {
	      case Some((item, _)) =>
	        item.equipped = true
	        equipped ::= item
	      case None => Unit
    	}
  }
}