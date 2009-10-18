// 
//  enum.scala
//  scala-examples
//  
//  Created by Lars Yencken on 2009-10-18.
//  Copyright 2009 Lars Yencken. All rights reserved.
// 

object Breed extends Enumeration {
    val doberman = Value("Doberman Pinscher")
    val yorkie = Value("Yorkshire Terrier")
    val scottie = Value("Scottish Terrier")
    val dane = Value("Great Dane")
    val portie = Value("Portugeuese Water Dog")
}

println("ID\tBreed")
for (breed <- Breed) println(breed.id + "\t" +  breed)

println("\nJust Terriers:")
Breed.filter(_.toString.endsWith("Terrier")).foreach(println)
