// 
//  simple-actor-script.scala
//  scala-examples
//  
//  Created by Lars Yencken on 2009-10-18.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

import scala.actors.Actor
import scala.actors.Actor._

class Redford extends Actor {
    def act() {
        println("A lot of what acting is, is paying attention.")
    }
}

val robert = new Redford
robert.start

var paulNewman = actor {
    println("To be an actor, you have to be a child.")
}
