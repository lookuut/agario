package com.agario.actions

import com.agario.commands.Command

trait Action {

  def run () : Command
  def isEnd () : Boolean
}
