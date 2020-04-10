// count the number of newlines in a file

'use strict'

const fs = require('fs')

function usage() {
    console.log('Usage: node 03-my-first-io.js <filename>')
    process.exit(1)
}

function countLines(filename) {
    const content = fs.readFileSync(filename, 'utf8')
    let lines = content.split('\n').length
    return lines - 1;
}

function main() {
    if (process.argv.length == 3) {
        const filename = process.argv[2]
        const lines = countLines(filename)
        console.log(lines)
    } else {
        usage()
    }
}

main()
