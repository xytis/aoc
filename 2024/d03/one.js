import {read} from "../lib/read.js";

// const input = read("./test.txt")
const input = read("./input.txt")

const scan = /mul\(\d{1,3},\d{1,3}\)/g;
const ectract = /^mul\((\d{1,3}),(\d{1,3})\)$/;

const matches = input.match(scan);

const sum = matches
    .map(expr => ectract.exec(expr))
    .map(([_, a, b]) => parseInt(a, 10) * parseInt(b, 10))
    .reduce((a, b) => a + b, 0);

console.log(sum);
