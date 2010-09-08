package main

import (
    "./freq"
    "fmt"
)

func main() {
    dist := freq.NewFreqDist()
    dist.Load("counts")
    keys := []string{"日", "高", "秋"}
    for i := 0; i < len(keys); i++ {
        fmt.Printf("%s %f\n", keys[i], dist.Prob(keys[i]))
    }
}
