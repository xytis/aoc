import {readLines} from "../lib/read.js";

const lines = readLines('./input.txt');

const pairs = lines.map(l => l.split(/\s+/))

const unzipped = pairs.reduce(function(acc, pair) {
    const [left, right] = acc;
    const [x, y] = pair;
    left.push(x);
    right.push(y);
    return acc;
}, [[], []]);

const [left, right] = unzipped;

// Remove white space
left.pop()
right.pop()

// Convert right list into accumulator
const counts = right.reduce(function(acc, el) {
    acc[el] = acc[el] ? acc[el] + 1 : 1;
    return acc;
}, new Map())

const calc = left.reduce(function(acc, el) {
    const mult = counts[el] ? counts[el] : 0;
    return acc + mult * el;
}, 0)

console.log(calc)
