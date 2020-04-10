// time-server.js <port>
//
// Listens to connections on a given port, then writes a 
// formated datetime to the socket followed by a newline.

const net = require('net')
const strftime = require('strftime')

const port = Number.parseInt(process.argv[2])

const server = net.createServer(function listener(socket) {
    let time = strftime('%Y-%m-%d %H:%M')
    socket.end(time + '\n')
})
server.listen(port)