package main

import (
    "fmt"
    "os"
)

// Note: incorrect handling of boundary cases, and doesn't handle newlines
// in the same way as other code.
func main() {
    bufSize := 4096
    buf := make([]byte, bufSize)
    for n, err := os.Stdin.Read(buf); err == nil && err != os.EOF; 
            n, err = os.Stdin.Read(buf) {
        start := 0
        for i, c := range buf[0:n] {
            if c == ' ' || c == '\n' {
                fmt.Printf("%s ", buf[start:i])
                start = i + 1;
            }
        }
        n = 0
    }
}
