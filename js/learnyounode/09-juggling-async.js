// juggling-async <url1> <url2> <url3>
//
// Print out the contents of a HTTP GET on each URL, in the order
// that they were given on the command-line, but fetching them
// in an async manner.
//

'using strict'

const http = require('http')
const https = require('https')
const concat = require('concat-stream')

function get(url, callback) {
    const method = (url.startsWith('https') ? https.get : http.get)
    method(url, (resp) => {
        resp.setEncoding('utf8')
        resp.on('error', console.error)
        resp.pipe(concat((data) => {
            callback(null, data)
        }))
    }).on('error', console.error)
}

/**
 * Map the method over xs by chaining callbacks.
 */
function sequentially(xs, method, callback) {
    if (!xs.length) {
        return
    }

    method(xs[0], (err, data) => {
        callback(err, data)
        sequentially(xs.slice(1), method, callback)
    })
}

const urls = [
    process.argv[2],
    process.argv[3],
    process.argv[4],
]

sequentially(urls, get, (err, data) => {
    if (err) {
        return console.log(err)
    }
    console.log(data)
})