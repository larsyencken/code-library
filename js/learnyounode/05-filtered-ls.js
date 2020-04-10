// filtered-ls <ext>
//
// Lists all files in the current directory that end with .<ext>

'use strict'

const fs = require('fs')
const path = require('path')

const dirname = process.argv[2];
const ext = '.' + process.argv[3];

fs.readdir(dirname, function (err, files) {
    if (err) {
        return console.error(err)
    }
    files.forEach(function (f) {
        if (path.extname(f) == ext) {
            console.log(f)
        }
    })
})
