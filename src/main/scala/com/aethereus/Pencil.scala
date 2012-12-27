package com.aethereus

import akka.actor._
import akka.pattern.ask
import akka.util.duration._
import akka.util.Timeout
import java.net.InetSocketAddress
import akka.util.ByteString

class TCPServer(port: Int) extends Actor {
  override def preStart {
    IOManager(context.system) listen new InetSocketAddress(port)
  }
  
  def receive = {
    case IO.NewClient(server) =>
      context.actorOf(Props(new Player(server)))
  }
}

object TCPServer extends App
{
  val port = Option(System.getenv("PORT")) map (_.toInt) getOrElse 8080
  ActorSystem().actorOf(Props(new TCPServer(port)))			
}
