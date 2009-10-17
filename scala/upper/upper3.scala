// 
//  upper3.scala
//  upper
//  
//  Created by Lars Yencken on 2009-10-17.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

object Upper {
    def main(args: Array[String]) = {
        args.map(_.toUpperCase()).foreach(printf("%s ", _))
        println("")
    }
}
