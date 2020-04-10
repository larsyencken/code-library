'use strict'

const fs = require('fs')
const path = require('path')

module.exports = findFiles

function findFiles(dirName, ext, callback) {
    fs.readdir(dirName, function (err, files) {
        if (err) {
            return callback(err)
        }

        let matches = []
        for (let i = 0; i < files.length; i++) {
            const f = files[i];
            if (path.extname(f) == '.' + ext) {
                matches.push(f)
            }
        }

        callback(null, matches)
    })
}
