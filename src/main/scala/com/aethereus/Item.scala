package com.aethereus

import scala.util.Random
import akka.actor.ActorRef

object ItemRegistry {
  var items: Set[(String, () => Item)] =
    Set(("WoodenSword", () => WoodenSword),
      ("ReverseMirror", () => ReverseMirror))
}

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

  def canInteract(command: String): Boolean = false
  def interact(command: String, actorRef: ActorRef) = {}
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

  val setDescription = "set description (.*)".r
  val setRace = "set race (.*)".r

  override def canInteract(command: String): Boolean = {
    command match {
      case "look mirror" |
        setDescription(_) |
        setRace(_) =>
        return true
      case _ =>
        return false
    }
  }

  override def interact(command: String, actorRef: ActorRef) = {
    command match {
      case "look mirror" =>
        actorRef ! Write("You notice an ornate mirror, sitting heavily on the ground. " +
          "Upon further inspection, you see that it is a [87 magic mirror], to which " +
          "you describe yourself, and it will make it so.\r\n\r\n" +
          "To use: [118 set description {description}] to describe yourself, and " +
          "[118 set race {race}] to set your race.")
      case setDescription(description) =>
        actorRef ! SetDescription(description)
        actorRef ! Write("You've changed a bit about your [118 look], and you decide you're pretty " +
          "pleased with the progress you've been making in your life.")
      case setRace(race) =>
        val r = race.toLowerCase()
        val articledName = Races.registry.find(_._1 == r) match {
          case Some((r, race)) =>
            actorRef ! SetRace(r)
            actorRef ! Write("You feel [118 faint], and the room seems to swirl around you. " +
              "You [120 stagger] around a bit, but when you get your bearings, you see your " +
              s"new self looking back.\r\n\r\n[118 You are now ${race.nameWithArticle}")
          case None =>
            actorRef ! Write("That's not a race!")

        }
    }
  }
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