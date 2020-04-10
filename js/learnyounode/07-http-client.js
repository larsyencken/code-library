// http-client <url>
//
// Log to the console each data event from a GET request to the URL.
//

const http = require('http')
const https = require('https')

const url = process.argv[2]

const get = (url.startsWith('https') ? https.get : http.get)

get(url, (resp) => {
    resp.setEncoding('utf8')
    resp.on('data', console.log)
    resp.on('error', console.error)
}).on('error', console.error)

