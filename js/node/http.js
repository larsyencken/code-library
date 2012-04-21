// vim: ts=2 sw=2 sts=2 et :

var http = require("http");
var puts = require("sys").puts

http.createServer(function (req, res) {
    res.writeHead(200, {"Content-Type": "text/plain"});
    res.write("Hello\n");
    res.write("World\n");
    res.end();
  }).listen(8000);
puts("Listening at http://localhost:8000/");
