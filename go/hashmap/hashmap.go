/*
    A hashtable which works for generic item types.
 */
package hashmap

import (
    "container/vector"
    "hash/adler32"
)

type Hashable interface {
    Hash() uint32
    Equals(rhs interface{}) bool
    Less(rhs interface{}) bool
}

type HashString string

func (s HashString) Hash() uint32 {
    return adler32.Checksum(s)
}

func (s *HashString) Equals(rhs interface{}) bool {
    return s == rhs.(string)
}

func (s *HashString) Less(rhs interface{}) bool {
    return s < rhs.(string)
}

type KeyValue struct {
    key *Hashable
    value interface{}
}

func Pack(key *Hashable, value interface{}) *KeyValue {
    kv := new(KeyValue)
    kv.key = key
    kv.value = value
    return kv
}

type Map map[uint32]*vector.Vector

func (table *Map) Set (key *Hashable, value interface{}) bool {
    hash := key.Hash()
    existing, ok := (*table)[hash]
    if ok {
        // linear scan through the chain
        i := find(existing, key)
        if i >= 0 {
            existing.Set(i, Pack(key, value))
            return true
        }

        existing.Push(Pack(key, value))

    } else {
        v := new(vector.Vector)
        v.Push(Pack(key, value))
        (*table)[hash] = v
    }
    return false
}

func find(v *vector.Vector, key *Hashable) int {
    for i := 0; i < v.Len(); i++ {
        if key.Equals(v.At(i).(KeyValue).key) {
            return i
        }
    }
    return -1
}

func (table *Map) Get (key *Hashable) (interface{}, bool) {
    hash := key.Hash()
    v, ok := (*table)[hash]
    if ok {
        i := find(v, key)
        if i >= 0 {
            return v.At(i), true
        }
    }
    return nil, false
}

