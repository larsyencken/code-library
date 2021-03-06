package lines

import (
    "os"
    "io"
)

func OpenAndIterate(filename string) (chan string, os.Error) {
    f, err := os.Open(filename)
    if err != nil {
        return nil, err
    }

    ch := make(chan string, 20)
    go func() {
        Iterate(f, ch)
        f.Close()
        close(ch)
    }()

    return ch, err
}

/**
 * Returns a channel over lines, or nil if an error occurs opening the given
 * file.
 */
func Iterate(f io.Reader, ch chan string) {
    maxSize := 4096

    buf := make([]byte, maxSize)
    offset := 0

    // for each block of maxSize bytes
    for n, err := f.Read(buf); ; n, err = f.Read(buf[offset:maxSize]) {
        if err != nil && err != os.EOF {
            panic(err.String())
        }
        n = offset + n

        // for each newline char found
        for i := find(buf[0:n], '\n'); i != -1; i = find(buf[0:n], '\n') {
            // output line at buf[0:i] (ignoring newline at buf[i])
            ch <- string(newCopy(buf[0:i]))

            copy(buf, buf[i + 1:n])
            n -= i + 1
        }

        // no more newlines in the buffer, which is still filled

        if n == maxSize {
            // no newlines within maxsize, just send it as a sentence
            ch <- string(newCopy(buf))
            n = 0
        }

        // still filled from [0:n]
        offset = n

        if err == os.EOF {
            // last chunk, flush it
            if n > 0 {
                ch <- string(buf[0:n])
            }
            break
        }
    }
}

func newCopy(buf []byte) []byte {
    c := make([]byte, len(buf))
    copy(c, buf)
    return c
}

func find(buf []byte, c byte) int {
    for i, b := range(buf) {
        if b == c {
            return i
        }
    }
    return -1
}

