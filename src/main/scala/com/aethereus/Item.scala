package com.aethereus

import scala.util.Random

abstract class Item {
  var equipped = false
  val name: String
  val description: String

  val shortName: String

  val hpBonus: Int
  val mpBonus: Int

  val strengthBonus: Int
  val dexterityBonus: Int
  val intelligenceBonus: Int

  val armorBonus: Int
  val speedBonus: Int

  val maxDamageOffset: Int
  val minDamage: Int
}

object WoodenSword extends Item {
  val name = "Wooden Sword"
  val shortName = "sword"

  val description = "A small practice sword."

  val hpBonus = 0
  val mpBonus = 0

  val strengthBonus = 1
  val dexterityBonus = 0
  val intelligenceBonus = 0

  val armorBonus = 0
  val speedBonus = 0

  val maxDamageOffset = 2
  val minDamage = 1
}

object ReverseMirror extends Item {
  val name = "A reverse mirror"
  val shortName = "mirror"

  val description = "This item allows you to tell it what you look like"

  val hpBonus = 0
  val mpBonus = 0

  val strengthBonus = 1
  val dexterityBonus = 0
  val intelligenceBonus = 0

  val armorBonus = 0
  val speedBonus = 0

  val maxDamageOffset = 2
  val minDamage = 1
}

trait Inventory {
  var inventory: List[(Item, Int)] = List()
  var equipped: List[Item] = List()

  val inventoryRandom = new Random()

  def printInventory(): String = {
    var inventoryString = ""
    for ((item, count) <- inventory) {
      if (item.equipped) inventoryString += "* "
      inventoryString += item.name + " x" + count + "\r\n"
    }
    return inventoryString
  }

  def addToInventory(newItem: Item) {
    inventory.find(x => x._1 == newItem) match {
      case Some(x) =>
        inventory = inventory diff List(x)
        inventory ::= (newItem, x._2 + 1)
      case None =>
        inventory ::= (newItem, 1)
    }
  }

  def damageRoll(): Int = {
    if (equipped.length < 1) return 1 //Your fists do one damage

    // TODO: Make this not wrong.
    val maxDamage = equipped.map(item => item.maxDamageOffset).max
    val minDamage = equipped.map(item => item.minDamage).max
    return inventoryRandom.nextInt(maxDamage) + minDamage
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
        val n = itemName.toLowerCase()
        item.name.toLowerCase() == n || item.shortName.toLowerCase() == n
      }) match {
      case Some((item, _)) =>
        item.equipped = true
        equipped ::= item
      case None => Unit
    }
  }
}