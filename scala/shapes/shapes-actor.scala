// 
//  shapes-actor.scala
//  scala
//  
//  Created by Lars Yencken on 2009-10-17.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

package shapes {
    import scala.actors._
    import scala.actors.Actor._
    
    object ShapeDrawingActor extends Actor {
        def act() {
            loop {
                receive {
                    case s: Shape   => s.draw()
                    case "exit"     => println("exiting..."); exit
                    case x: Any     => println("Error: Unknown message! " + x)
                }
            }
        }
    }
}