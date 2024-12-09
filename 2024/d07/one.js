import {readLines} from "../lib/read.js";

// const input = readLines("test1.txt")
const input = readLines("input.txt")

const cases = input.map((line) => {
    const [sum, rest] = line.split(":").map(a => a.trim())

    const numbers = rest.split(' ').map(a => parseInt(a, 10));

    return [parseInt(sum, 10), numbers];
})

// console.log(cases);

function works(target, [a, b, ...rest]) {
    if (rest.length === 0) {
        return a*b === target || a+b === target;
    }
    return works(target, [a*b, ...rest]) || works(target, [a+b, ...rest]);
}

const results = cases.map(([target, numbers]) => [works(target, numbers), target]);

// console.log(results);

const sum = results.reduce((sum, [include, val]) => include ? sum+val : sum, 0)

console.log(sum)
