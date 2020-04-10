/**
 * http-json-api-server <port>
 */

'using strict'

const http = require('http')
const url = require('url')
const querystring = require('querystring')

const port = Number(process.argv[2])

// surprising it seems to expect a global here
let currentTime = Date.now();

const routes = {
    "/api/unixtime": unixtime,
    "/api/parsetime": parsetime
}

const server = http.createServer((req, resp) => {
    const path = url.parse(req.url).pathname
    console.log(`${req.method} ${path}`)
    if (req.method == 'GET' && path in routes) {
        routes[path](req, resp)
    } else {
        resp.writeHead(404)
        resp.end()
    }
})

server.listen(port)

function unixtime(req, resp) {
    const obj = { 'unixtime': currentTime.getTime() }
    resp.writeHead(200, { 'Content-Type': 'application/json' })
    resp.end(JSON.stringify(obj))
}

function parsetime(req, resp) {
    const query = querystring.parse(url.parse(req.url).query)
    if ('iso' in query) {
        const iso = query['iso']
        currentTime = new Date(iso)
        const obj = {
            'hour': currentTime.getHours(),
            'minute': currentTime.getMinutes(),
            'second': currentTime.getSeconds()
        }
        resp.writeHead(200, { 'Content-Type': 'application/json' })
        resp.end(JSON.stringify(obj))
    } else {
        resp.writeHead(400, 'needs iso parameter in get arguments')
        resp.end()
    }
}