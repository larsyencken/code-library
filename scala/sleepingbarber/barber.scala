// 
//  barber.scala
//  scala-examples
//  
//  Created by Lars Yencken on 2009-10-18.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

package sleepingbarber

import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random

class Barber extends Actor {
    private val random = new Random()
    
    def helpCustomer(customer: Customer) {
        if (self.mailboxSize >= 3) {
            println("[b] not enough seats, turning customer " + 
                    customer.id + " away")
        } else {
            println("[b] cutting hair of customer " + customer.id)
            Thread.sleep(100 + random.nextInt(400))
            customer ! Haircut
        }
    }
    
    def act() {
        loop {
            react {
                case customer: Customer => helpCustomer(customer)
            }
        }
    }
}
