package com.aethereus

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.net.InetSocketAddress
import akka.util.ByteString
import org.neo4j.graphdb.factory._
import org.neo4j.scala.SingletonEmbeddedGraphDatabaseServiceProvider
import org.neo4j.scala.Neo4jIndexProvider
import org.neo4j.scala.RestGraphDatabaseServiceProvider
import java.net.URI
import scala.collection.mutable.Stack

object Color {
  def parse(str: String) = {
    var stack: Stack[Int] = new Stack()
    var currentParseColor = "";
    var currentEmittedColor = 231;
    var state = ""

    var currentString = ""

    for (c <- str) {
      if (state == "") {
        if (c == '[') {
          stack.push(currentEmittedColor)
          currentEmittedColor = 0;
          state = "findingColor"
        } else if (c == ']') {
          currentString += s"\u001B[38;5;${stack.pop()}m"
        } else {
          currentString += c
        }
      } else if (state == "findingColor") {
        if (c == ' ') {
          state = ""
          currentEmittedColor = currentParseColor.toInt
          currentString += s"\u001B[38;5;${currentEmittedColor}m"
          currentParseColor = ""
        } else {
          currentParseColor += c;
        }
      }
    }

    currentString
  }
}

class TCPServer(port: Int) extends Actor with RestGraphDatabaseServiceProvider with Neo4jIndexProvider {
  def uri = new URI("http://localhost:7474/db/data/")

  Console.println("set up index config")
  override def NodeIndexConfig = ("RoomIndex", Some(Map("provider" -> "lucene", "type" -> "fulltext"))) :: Nil

  override def preStart {
    IOManager(context.system) listen new InetSocketAddress(port)
  }

  val roomService = context.actorOf(Props(new RoomService()), name = "RoomService")

  def receive = {
    case IO.NewClient(server) =>
      context.actorOf(Props(new Player(server, context.actorFor("RoomService/Start"))))
  }
}

object Pencil extends App with RestGraphDatabaseServiceProvider {
  def uri = new URI("http://localhost:7474/db/data/")

  /*
  Runtime.getRuntime().addShutdownHook(new Thread () {
    Console.println("shutdown hook triggered");
    override def run = {
      ds.gds.shutdown()
    }
  })
  */

  val port = Option(System.getenv("PORT")) map (_.toInt) getOrElse 8080
  ActorSystem().actorOf(Props(new TCPServer(port)))
}
