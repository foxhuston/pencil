package com.aethereus

import akka.actor._
import scala.util.Random

class RoomService extends Actor {
  def receive = {
    case NewRoom(name) =>
      Console.println("Creating " + name)
      context.actorOf(Props(new Room(name)), name = name)
  }
} 

class Room(var name: String) extends Actor {
  var description = ""
  var exits: Set[(String, String)] = Set()
  var Inhabitants: Set[ActorRef] = Set()
  
  val random = new Random()
  
  Console.println(context.self.path)
  
  def Leave(sender: ActorRef) = {
    Inhabitants -= sender
    for(p <- Inhabitants) p ! LeaveMessage(sender)
  }
  
  def receive = partA orElse partB
  
  val partA: PartialFunction[Any, Unit] = {
    case Enter =>
      for(p <- Inhabitants) p ! JoinMessage(context.sender) 
      writeEnterMessage(context.sender)
      Inhabitants += context.sender
    case Leave =>
      Leave(context.sender)
    case Say(who, what) =>
      for(p <- Inhabitants) p ! Write(who + ": " + what)
    case Description =>
      context.sender ! Write(description)
    case SetDescription(newDescription) =>
      description = newDescription
    case EnterMessage =>
      writeEnterMessage(context.sender)
    case Exits =>
      context.sender ! ExitMessage(exits)
    case AddExit(exit) =>
      exits += exit
    case Attack(who, what, how, roll, attackRoll) =>
      for(p <- Inhabitants) p ! Attack(who, what, how, roll, attackRoll)
    case LeaveBy(direction) =>
      exits.find(_._1 == direction) match {
        case Some(t) =>
          Leave(context.sender)
          context.sender ! LeaveOk(t)
        case None =>
          context.sender ! LeaveFail
      }
  }
  
  val partB: PartialFunction[Any, Unit] = {
    case RandomRoomInhabitant =>
      val x = random.nextInt(Inhabitants.count(_ => true))
      val ref = Inhabitants.drop(x).head
      context.sender ! RandomRoomInhabitantResponse(ref)
  }
  
  def writeEnterMessage(sender: ActorRef) = {
    sender !
      	Write(name
      	    + " (" + Inhabitants.count(_ => true) + ")\r\n"
      	    + "Exits: "
      	    + exits.map(_._1).mkString(",")
      	    + "\r\n"
      	    + description)
  }
}