// 
//  customer.scala
//  scala-examples
//  
//  Created by Lars Yencken on 2009-10-18.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

package sleepingbarber

import scala.actors.Actor
import scala.actors.Actor._

case object Haircut

class Customer(val id: Int) extends Actor {
    var shorn = false
    
    def act() = {
        loop {
            react {
                case Haircut => {
                    shorn = true
                    println("[c] customer " + id + " got a haircut")
                }
            }
        }
    }
}
