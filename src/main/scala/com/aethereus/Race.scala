package com.aethereus

object Races {
  val registry: Set[(String, Race)]
      = Set(("halfling", Halfling),
            ("elf", Elf),
            ("dwarf", Dwarf),
            ("human", Human),
            ("half-elf", HalfElf),
            ("gnome", Gnome),
            ("orc", Orc),
            ("satyr", Satyr))
}

abstract class Race {
  val name: String
  val description: String
  
  def modifyDamage(input: Damage): Damage = {
    return input
  }
  
  def modifyAttack(input: AttackRoll): AttackRoll = {
    return input
  }
}

object Halfling extends Race {
  val name = "Halfling"
  val description = "Watch the Hobbit."
}

object Elf extends Race {
  val name = "Elf"
  val description = "Elfy"
}

object Dwarf extends Race {
  val name = "Dwarf"
  val description = "Short & Stout." 
}

object Human extends Race {
  val name = "Human"
  val description = "Well..."
}

object HalfElf extends Race {
  val name = "Half Elf"
  val description = "You are what you eat."
}

object Gnome extends Race {
  val name = "Gnome"
  val description = "Small humaniod creature that lives underground.  Often they wear pointy hats."
}

object Orc extends Race {
  val name = "Orc"
  val description = "Brutish, aggressive, and repulsive."
}

object Satyr extends Race {
  val name = "Satyr"
  val description = "Those goat-legged fellows."
}
