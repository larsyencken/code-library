package main

import (
    "fmt"
)

/**
 * Generate a stream of integers.
 */
func generate() chan int {
    ch := make(chan int)
    go func() {
        for i := 2; ; i++ {
            ch <- i
        }
    }()
    return ch
}

/**
 * Filter out divisors of a prime from a stream.
 */
func filter(in chan int, prime int) chan int {
    out := make(chan int)
    go func()  {
        for {
            i := <-in
            if i % prime != 0 {
                out <- i
            }
        }
    }()
    return out
}

/**
 * Generate a stream of primes.
 */
func sieve() chan int {
    out := make(chan int)
    go func() {
        ch := generate()
        for {
            prime := <-ch
            out <- prime
            ch = filter(ch, prime)
        }
    }()
    return out
}

func main() {
    ch := sieve()
    for {
        prime := <-ch
        fmt.Printf("%d\n", prime)
    }
}
