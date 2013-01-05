package com.aethereus

import akka.actor._

case class Write(message: String)
case class Read(message: String)

case class JoinMessage(player: ActorRef)
case class SendMeAJoinMessage

case class LeaveMessage(player: ActorRef)
case class SendMeALeaveMessage

case class Enter
case class Leave
case class LeaveBy(direction: String)
case class LeaveOk(exit: (String, String))
case class LeaveFail

case class Say(nick: String, input: String)

case class Attack(nick: String, what: String, how: String, roll: Int, attackRoll: Int)
case class ReportHit(who: String, damage: Int)
case class ReportMiss(who: String)

case class Description
case class SetDescription(description: String)

case class Exits
case class ExitMessage(exits: Set[(String, String)])
case class AddExit(exit: (String, String))

case class EnterMessage

case class NewRoom(name: String)