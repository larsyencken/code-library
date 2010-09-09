package freq

import (
    "fmt"
    "os"
    "bufio"
    "strconv"
    "strings"
    "./hashmap"
)

type MultiFreqDist struct {
    counts *hashmap.Map
    size int
}

func NewMultiFreqDist() *MultiFreqDist {
    result := new(MultiFreqDist)
    result.counts = new(hashmap.Map)
    result.size = 0

    return result
}

func (dist *MultiFreqDist) Inc(k *hashmap.Hashable) {
    v, ok := dist.counts.Get(k)
    if ok {
        dist.counts.Set(k, v.(int) + 1)
    } else {
        dist.counts.Set(k, 1)
    }
    dist.size++
}

func (dist *MultiFreqDist) Prob(k *hashmap.Hashable) float {
    count, _ := dist.counts.Get(k)
    return float(count.(int)) / float(dist.size)
}

func (dist *MultiFreqDist) Load(filename string) {
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
        dist.counts.Set(key, count)
        dist.size += count
    }
}

