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
    "container/vector"
)

type Tokenizer interface {
    Tokenize(s string) *vector.StringVector
}

type WhitespaceTokenizer struct {}

func (tr WhitespaceTokenizer) Tokenize(s string) *vector.StringVector {
    v := new(vector.StringVector)
    offset := 0
    for i, c := range(s) {
        if isSpace(c) {
            l := i - offset
            if l > 0 {
                v.Push(s[offset:i])
            }
            offset = i + 1
        }
    }
    if len(s) - offset > 0 {
        v.Push(s[offset:len(s)])
    }
    return v
}

func isSpace(b int) bool {
    return b == ' ' || b == '\t' || b == '\n'
}

type RegexpTokenizer struct {
    expr *regexp.Regexp
}

func (tr RegexpTokenizer) Tokenize(s string) *vector.StringVector {
    v := new(vector.StringVector)
    ms := tr.expr.FindAllString(s, 0)
    for _, m := range ms {
        v.Push(m)
    }
    return v
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

    for line := range ls {
        v := tr.Tokenize(line)
        for i := 0; i < v.Len(); i++ {
            tok := v.At(i)
            fmt.Printf(tok)
            if i + 1 == v.Len() {
                fmt.Printf("\n")
            } else {
                fmt.Printf(" ")
            }
        }
    }
    fmt.Printf("\n")
}
