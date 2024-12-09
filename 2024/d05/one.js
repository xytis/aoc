import {readLines} from "../lib/read.js";

// const lines = readLines('./test1.txt');
const lines = readLines('./input.txt');

const [rules, printouts] = lines.reduce(([rules, printouts, s], line) => {
    if (line === "") {
        return [rules, printouts, true];
    }
    if (s) {
        printouts.push(line)
    } else {
        rules.add(line)
    }
    return [rules, printouts, s]
}, [new Set(), [], false])


const cases = printouts.map((line) => {
    const pages = line.split(',').map(p => parseInt(p, 10));
    const inverseFacts = new Set();
    for (let i = 0; i < pages.length-1; i++) {
        for (let j = i + 1; j < pages.length; j++) {
            inverseFacts.add(`${pages[j]}|${pages[i]}`);
        }
    }
    const middle = pages[Math.floor((pages.length - 1) / 2)];
    return [pages, inverseFacts, middle];
})

const correct = cases.filter(([_, inverseFacts, middle]) => {
    return inverseFacts.intersection(rules).size === 0
});

const result = correct.reduce((res, [_1, _2, middle]) => res + middle, 0);

console.log(result)

