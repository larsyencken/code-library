package freq

import (
    "fmt"
    "os"
    "bufio"
    "strconv"
    "strings"
)

type FreqDist struct {
    counts map[string]int
    size int
}

func NewFreqDist() *FreqDist {
    result := new(FreqDist)
    result.counts = make(map[string]int)
    result.size = 0

    return result
}

func (dist *FreqDist) Inc(s string) {
    dist.counts[s]++
    dist.size++
}

func (dist *FreqDist) Prob(s string) float {
    count, _ := dist.counts[s]
    return float(count) / float(dist.size)
}

func (dist* FreqDist) Load(filename string) {
    file, _ := os.Open(filename, os.O_RDONLY, 0666)
    if file == nil {
        panic("cannot open file")
    }
    defer file.Close()
    reader := bufio.NewReader(file)
    for line, err := reader.ReadString('\n'); err != os.EOF ; 
            line, err = reader.ReadString('\n') {
        line = line[0:len(line) - 1]
        parts := strings.Split(line, " ", -1)
        if len(parts) != 2 {
            panic(fmt.Sprintf("file has bad format: %s", filename))
        }
        key := parts[0]
        count, converr := strconv.Atoi(parts[1])
        if (converr != nil) {
            panic(converr.String())
        }
        dist.counts[key] = count
        dist.size += count
    }
}
