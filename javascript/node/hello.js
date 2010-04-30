// vim: ts=2 sw=2 sts=2 et :

puts = require("sys").puts

setInterval(function () {
    puts("hello");
  }, 500);

process.addListener("SIGINT", function () {
    puts("goodbye");
    process.exit(0);
  });
