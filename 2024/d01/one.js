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

left.sort((a, b) => a - b);
right.sort((a, b) => a - b);

function zip(a, b) {
    return a.map(function(e, i) {
        return [e, b[i]];
    });
}

function sum(a) {
    return a.reduce(function(a, b) {
        return a + b;
    }, 0)
}

const zipped = zip(left, right);

const calc = zipped.map(function([l, r]) {
    return Math.abs(l-r);
})

console.log(sum(calc));
