package com.aethereus

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

          try {
            currentEmittedColor = currentParseColor.toInt
          } catch {
            case _ : NumberFormatException =>
              currentEmittedColor = 231
          }

          currentString += s"\u001B[38;5;${currentEmittedColor}m"
          currentParseColor = ""
        } else {
          currentParseColor += c;
        }
      }
    }

    currentString + "\u001B[38;5;231m"
  }
}
