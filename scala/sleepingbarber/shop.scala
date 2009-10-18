// 
//  shop.scala
//  scala-examples
//  
//  Created by Lars Yencken on 2009-10-18.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

package sleepingbarber

import scala.actors.Actor
import scala.actors.Actor._

class Shop extends Actor {
    val barber = new Barber()
    barber.start
    
    def act() {
        println("[s] the shop is open")
        
        loop {
            react {
                case customer: Customer => barber ! customer
            }
        }
    }
}
