// count the number of newlines in a file with async

'use strict'

const fs = require('fs')

function usage() {
    console.log('Usage: node 04-my-first-async-io.js <filename>')
    process.exit(1)
}

function countLines(filename) {
    fs.readFile(filename, 'utf8', function doneReading(err, content) {
        if (err) {
            return console.log(err)
        }
        const lines = content.split('\n').length - 1
        console.log(lines)
    })
}

function main() {
    if (process.argv.length == 3) {
        const filename = process.argv[2]
        countLines(filename)
    } else {
        usage()
    }
}

main()
