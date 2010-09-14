/*
 *  tokenize.go
 */

package main

import (
    "fmt"
    "regexp"
    "container/vector"
)

type Tokenizer interface {
    Tokenize(s string) *vector.StringVector
    TokenizeChannel(chan string) chan string
}

type RegexpTokenizer struct {
    expr *regexp.Regexp
}

func (tr RegexpTokenizer) Tokenize(s string) *vector.StringVector {
    v := new(vector.StringVector)
    ch := tr.expr.AllMatchesStringIter(s, 0)
    for t := range ch {
        v.Push(t)
    }
    return v
}

func (tr RegexpTokenizer) TokenizeChannel(input chan string) chan string {
    c := make(chan string, 40)
    go func() {
        s := <-input
        for t := range tr.expr.AllMatchesStringIter(s, 0) {
            c <- t
        }
    }()
    return c
}

func WordPunctTokenizer() Tokenizer {
    pattern, err := regexp.Compile("r'[a-zA-Z]+|[^A-Za-z \t]+'")
    if (err != nil) {
        panic(err.String())
    }
    return Tokenizer(RegexpTokenizer{pattern})
}

func main() {
    tr := WordPunctTokenizer()
    c := make(chan string)
    c <- "The dog swam, then I said 'no'!"
    close(c)
    for tok := range tr.TokenizeChannel(c) {
        fmt.Printf(tok)
    }
    fmt.Printf("\n")
}
