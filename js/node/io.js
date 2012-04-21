// vim: ts=2 sw=2 sts=2 et :

var stat = require("fs").stat;
var puts = require("sys").puts;

var promise = stat("/etc/passwd");

promise.addCallback(function (s) {
    puts("modified: " + s.mtime);
  });
