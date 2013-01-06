package com.aethereus

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.net.InetSocketAddress
import akka.util.ByteString

class TCPServer(port: Int) extends Actor {
  override def preStart {
    IOManager(context.system) listen new InetSocketAddress(port)
  }
  
  val roomService = context.actorOf(Props[RoomService], name = "RoomService")
  
  roomService ! NewRoom("Start")
  
  context.actorFor("RoomService/Start") ! SetDescription("The starting room.")
  
  def receive = {
    case IO.NewClient(server) =>
      context.actorOf(Props(new Player(server, context.actorFor("RoomService/Start"))))
  }
}

object Pencil extends App
{
  val port = Option(System.getenv("PORT")) map (_.toInt) getOrElse 8080
  ActorSystem().actorOf(Props(new TCPServer(port)))			
}
