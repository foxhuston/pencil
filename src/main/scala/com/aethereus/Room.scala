package com.aethereus

import akka.actor._
import scala.util.Random
import org.neo4j.graphdb._
import org.neo4j.cypher.javacompat.ExecutionEngine
import org.neo4j.helpers.collection.IteratorUtil
import scala.collection.JavaConversions._
import org.neo4j.scala._
import java.net.URI

class RoomService() extends Actor with Neo4jWrapper with RestGraphDatabaseServiceProvider with Neo4jIndexProvider {
  def uri = new URI("http://localhost:7474/db/data/")
    
  Console.println("Getting Index")

  // The default getter thingy from neo4j-scala doesn't seem to work
  val roomIndex = ds.gds.index.forNodes("RoomIndex");

  Console.println("Finding loaded nodes")
  
  var startFound = false
    
  // Load from graph
  withTx {
    implicit neo =>
      val everything = getAllNodes(neo)
      //Console.println("found " + everything.count(_ => true) + " nodes.")
      
      for(node <- everything)
      {
        Console.println("Looking...")
        
        node.getProperty("name") match {
          case name: String =>
          	node.getProperty("description") match {
          	  case description: String =>
          	    if(name == "Start") {
          	      startFound = true;
          	    }
          	    var exits: Set[(String, String)] = Set()
          	    var relationships = node.getRelationships()
          	    for(rel <- relationships.filter(r => r.getStartNode() == node)) {
          	      val direction = rel.getProperty("direction").toString()
          	      val name = rel.getEndNode().getProperty("name").toString()
          	      exits += ((direction, name))
          	    }
          	    
          	    Console.println("Loaded " + name)
          	    context.actorOf(Props(new Room(name, description, node, exits)), name = name)
          	}
        }
      }
  }
  
  if(!startFound)
  {
    Console.println("Detected new DB")
    makeNewRoom("Start", "This is the starting room")
  }
  
  Console.println("makeNewRoom")
  // handle receive
  
  def makeNewRoom(name: String, description: String = "") = {
    Console.println("Creating " + name)
      withTx {
		  implicit neo => {
		    val roomNode = createNode
		    roomNode("type") = "Room"
		    roomNode("name") = name
		    roomNode("description") = description
		    roomIndex += (roomNode, "name", name)
		    context.actorOf(Props(new Room(name, description, roomNode)), name = name)
		    context.sender ! RoomCreated(name)
		  }
	  }
  }
  
  Console.println("listening for actor messages")
  
  def receive = {
    case NewRoom(name) =>
      Console.println("Creating " + name)
      makeNewRoom(name)
  }
} 

//Note: Want nouns up in here:
// You are in a kitchen. There is a [sink (screwdriver)] here.

class Room(var name: String, var description: String, val roomNode: Node, var exits: Set[(String, String)] = Set()) extends Actor with Neo4jWrapper with RestGraphDatabaseServiceProvider {
  Console.println("Here's " + name)
  var Inhabitants: Set[ActorRef] = Set()
  
  val random = new Random()
  
  def uri = new URI("http://localhost:7474/db/data/")
  val roomIndex = ds.gds.index.forNodes("RoomIndex");
  
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
      withTx {
        implicit neo =>
    	  roomNode("description") = newDescription
      }
    case EnterMessage =>
      writeEnterMessage(context.sender)
    case Exits =>
      context.sender ! ExitMessage(exits)
    case AddExit((direction, nameTo)) =>
      exits += ((direction, nameTo))
      withTx {
        implicit neo =>
          val nodeTo = roomIndex.get("name", nameTo).getSingle()
          if(nodeTo != null) {
	          val relation:Relationship = roomNode --> "EXITS_TO" --> nodeTo <
	          val _ = relation.setProperty("direction", direction)
          } else {
            Console.println("Epic faliure")
          }
      }
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
      val inh = Inhabitants - context.sender
      val x = random.nextInt(inh.count(_ => true))
      val ref = inh.drop(x).head
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