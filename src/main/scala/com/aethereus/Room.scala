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
  var Inhabitants: Set[ActorRef] = Set()
  
  Console.println(context.self.path)
  
  def receive = {
    case Enter =>
      for(p <- Inhabitants) p ! JoinMessage(context.sender) 
      writeEnterMessage(context.sender)
      Inhabitants += context.sender
    case Leave =>
      Inhabitants -= context.sender
      for(p <- Inhabitants) p ! LeaveMessage(context.sender)
    case Description =>
      context.sender ! Write(description)
    case SetDescription(newDescription) =>
      description = newDescription
    case EnterMessage =>
      writeEnterMessage(context.sender)
  }
  
  def writeEnterMessage(sender: ActorRef) = {
    sender !
      	Write(name
      	    + " (" + Inhabitants.count(_ => true) + ")\r\n"
      	    + "Exits: "
      	    + "\r\n\r\n"
      	    + description
      	    + "\r\n")
  }
}