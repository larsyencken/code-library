// vim: ts=2 sw=2 sts=2 et :

var http = require("http");

http.createServer(function (req, res) {
    res.sendHeader(200, {"Content-Type": "text/plain"});
    res.write("Hello\n");
    res.write("World\n");
    res.end();
  }).listen(8000);
