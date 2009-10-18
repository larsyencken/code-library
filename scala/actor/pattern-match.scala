// 
//  pattern-match.scala
//  scala-examples
//  
//  Created by Lars Yencken on 2009-10-18.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

import scala.actors.Actor
import scala.actors.Actor._

val fussyActor = actor {
    loop {
        receive {
            case s: String => println("I got a String: " + s)
            case i: Int => println("I got an Int: " + i.toString)
            case _ => println("I have no idea what I just got.")
        }
    }
}

fussyActor ! "hi there"
fussyActor ! 23
fussyActor ! 3.33