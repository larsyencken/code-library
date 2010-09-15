/*
 *  tokenize.go
 */

package main

import (
    "os"
    "io"
    "fmt"
    "regexp"
    "container/vector"
    "./lines"
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
        defer close(c)

        for s := range input {
            for t := range tr.expr.AllMatchesStringIter(s, 0) {
                c <- t
            }
            c <- "<EOL>"
        }
    }()
    return c
}

func WordPunctTokenizer() Tokenizer {
    pattern, err := regexp.Compile("[a-zA-Z]+|[0-9]+|[^A-Za-z \t]")
    if (err != nil) {
        panic(err.String())
    }
    return Tokenizer(RegexpTokenizer{pattern})
}

func Duplicate(src []byte) {
    dup := make([]byte, len(src))
    Copy(src, dup)
}

func Copy(src []byte, dest []byte) {
    for i := len(src) - 1; i >= 0; i-- {
        dest[i] = src[i]
    }
}

func main() {
    tr := WordPunctTokenizer()

    ls := make(chan string)
    go func() {
        lines.Iterate(io.Reader(os.Stdin), ls)
        close(ls)
    }()

    ch := tr.TokenizeChannel(ls)
    first := true
    for tok := range ch {
        if tok == "<EOL>" {
            fmt.Printf("\n")
            first = true
        } else {
            if !first {
                fmt.Printf(" ")
            }
            fmt.Printf("%s", tok)
            first = false
        }
    }
    fmt.Printf("\n")
}
