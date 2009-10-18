// 
//  spawn.scala
//  scala-examples
//  
//  Created by Lars Yencken on 2009-10-18.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

import scala.concurrent.ops._
import scala.util.Random

object SpawnExample {
    def main(args: Array[String]) {
        println("this will run synchronously")
        
        val random = new Random()
        spawn {
            Thread.sleep(random.nextInt(100))
            println("this will run asynchronously")
        }
        spawn {
            Thread.sleep(random.nextInt(100))
            println("so will this")
        }
    }
}
