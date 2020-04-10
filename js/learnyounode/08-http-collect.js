// http-collect <url>
//
// Logs the number of characters in the response, then the full response text
//

const http = require('http')
const https = require('https')
const concat = require('concat-stream')

const url = process.argv[2]

const get = (url.startsWith('https') ? https.get : http.get)

get(url, (resp) => {
    resp.setEncoding('utf8')
    resp.on('error', console.error)
    resp.pipe(concat((data) => {
        console.log(data.length)
        console.log(data)
    }))
}).on('error', console.error)
