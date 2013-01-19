package com.aethereus

import org.neo4j.graphdb.Node
import akka.actor._

case class Write(message: String)
case class Read(message: String)

case class JoinMessage(player: ActorRef)
case object SendMeAJoinMessage

case class LeaveMessage(player: ActorRef)
case object SendMeALeaveMessage



case class Enter(nick: String)
case class Leave(nick: String)
case class LeaveBy(nick: String, direction: String)
case class LeaveOk(exit: (String, String))
case object LeaveFail

case object RandomRoomInhabitant
case class RandomRoomInhabitantResponse(actor: ActorRef)

case object GetNick
case class GetNickResponse(nick: String)

case class Say(nick: String, input: String)

case class Attack(nick: String, what: String, damage: Damage)
case class ReportHit(who: String, damage: Int)
case class ReportMiss(who: String)
case object Died

case object Description
case class SetDescription(description: String)

case object Exits
case class ExitMessage(exits: Set[(String, String)])
case class AddExit(exit: (String, String))

case object EnterMessage

case class NewRoom(name: String)
case class RoomCreated(name: String)