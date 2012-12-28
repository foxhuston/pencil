package com.aethereus

case class Write(message: String)
case class Read(message: String)

case class JoinMessage(nick: String)

case class EnterMessage(room: Room)

case class Enter(player: Player)
case class Leave(player: Player)

case class Description
case class SetDescription(description: String)

case class NewRoom(name: String)