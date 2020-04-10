/**
 * http-uppercaserer <port>
 * 
 * Listens to POST requests and converts body characters to upper-case.
 */

'using strict'

const http = require('http')
const map = require('through2-map')

const port = Number(process.argv[2])

const toUpperCase = map((chunk) => {
    return chunk.toString().toUpperCase()
})

const server = http.createServer((req, resp) => {
    console.log(`${req.method} ${req.url}`)
    // handle other HTTP methods
    if (req.method != 'POST') {
        resp.writeHead(400, 'only accepts POST requests')
        resp.end()
    }
    req.pipe(toUpperCase).pipe(resp)
})

server.listen(port)