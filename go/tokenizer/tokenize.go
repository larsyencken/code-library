/*
 *  tokenize.go
 */

package main

import (
    "os"
    "io"
    "fmt"
    "regexp"
    "./lines"
)

type Tokenizer interface {
    Tokenize(s string) <-chan string
}

func TokenizeChannel(tr Tokenizer, input chan string) chan string {
    c := make(chan string, 40)
    go func() {
        defer close(c)

        for s := range input {
            for t := range tr.Tokenize(s) {
                c <- t
            }
            c <- "<EOL>"
        }
    }()
    return c
}

type WhitespaceTokenizer struct {}

func (tr WhitespaceTokenizer) Tokenize(s string) <-chan string {
    ch := make(chan string)
    go func() {
        defer close(ch)
        offset := 0
        for i, c := range(s) {
            if isSpace(c) {
                l := i - offset
                if l > 0 {
                    ch <- s[offset:i]
                }
                offset = i + 1
            }
        }
        if len(s) - offset > 0 {
            ch <- s[offset:len(s)]
        }
    }()
    return ch
}

func isSpace(b int) bool {
    return b == ' ' || b == '\t' || b == '\n'
}

type RegexpTokenizer struct {
    expr *regexp.Regexp
}

func (tr RegexpTokenizer) Tokenize(s string) <-chan string {
    ch := tr.expr.AllMatchesStringIter(s, 0)
    return ch
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
    tr := new(WhitespaceTokenizer)

    ls := make(chan string)
    go func() {
        lines.Iterate(io.Reader(os.Stdin), ls)
        close(ls)
    }()

    ch := TokenizeChannel(tr, ls)
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
