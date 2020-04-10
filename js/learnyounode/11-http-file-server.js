/**
 * http-file-server <port> <filename>
 * 
 * Serve HTTP requests on <port> and respond to each request with the contents of <filename>.
 */

'using strict'

const http = require('http')
const fs = require('fs')

const port = Number(process.argv[2])
const filename = process.argv[3]

const server = http.createServer((req, resp) => {
    resp.writeHead(200, { 'content-type': 'text/plain' })
    let stream = fs.createReadStream(filename)
    stream.pipe(resp)
})

server.listen(port)