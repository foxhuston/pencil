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

case class Description
case class SetDescription(description: String)

case class EnterMessage

case class NewRoom(name: String)