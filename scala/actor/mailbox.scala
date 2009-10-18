// 
//  mailbox.scala
//  scala-examples
//  
//  Created by Lars Yencken on 2009-10-18.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

import scala.actors.Actor
import scala.actors.Actor._

val countActor = actor {
    loop {
        react {
            case "how many?" => {
                println("I've got " + mailboxSize.toString + 
                        " messages in my mailbox.")
            }
        }
    }
}

countActor ! 1
countActor ! 2
countActor ! 3
countActor ! "how many?"
countActor ! "how many?"
countActor ! 4
countActor ! "how many?"
