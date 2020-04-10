// Lists all files in the current directory that end with .<ext>

'use strict'

const findFiles = require('./06-my-module')

const path = process.argv[2]
const ext = process.argv[3]

findFiles(path, ext, function (err, matches) {
    if (err) {
        return console.error(err)
    }
    matches.forEach(function (f) {
        console.log(f)
    })
})