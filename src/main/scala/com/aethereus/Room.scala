package com.aethereus

import akka.actor._

class RoomService extends Actor {
  def receive = {
    case NewRoom(name) =>
      context.actorOf(Props(new Room(name)), name = name)
  }
} 

class Room(var name: String) extends Actor {
  var description = ""

  var exits = Set()
  
  var Inhabitants: Set[Player] = Set()
  
  Console.println(context.self.path)
  
  def receive = {
    case Enter(player) =>
      for(p <- Inhabitants) p.self ! JoinMessage(player.nick)
      Inhabitants += player
    case Description =>
      context.sender ! Write(description)
    case SetDescription(newDescription) =>
      description = newDescription
  }
}