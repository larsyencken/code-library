// print the sum of arguments

'use strict'

const argv = process.argv.slice(2);

let total = 0;

for (let i = 0; i < argv.length; i++) {
    total += Number(argv[i]);
}

console.log(total)
