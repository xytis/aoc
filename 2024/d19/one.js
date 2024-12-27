import {read} from "../lib/read.js";
import {memo} from "../lib/utils.js";

const input = read("input.txt")
// const input = read("test.txt")

const [patternsInput, targetsInput] = input.split("\n\n")

const [patterns, max] = patternsInput.trim().split(", ").reduce((acc, pattern) => {
    acc[0].set(pattern, true)
    acc[1] = Math.max(acc[1], pattern.length)
    return acc
}, [new Map(), 0]);

const targets = targetsInput.trim().split("\n");

const match = memo((remainder) => {
    if (remainder.length === 0) {
        return true;
    }
    const branches = [];
    for (let len = Math.min(max, remainder.length); len > 0; len--) {
        const search = remainder.slice(0, len);
        if (patterns.has(search)) {
            branches.push(remainder.slice(len));
        }
    }
    for (const branch of branches) {
        if (match(branch)) {
            return true;
        }
    }
    return false;
})

console.log(targets.map((t, i) => {
    console.log(i, t);
    return match(t);
}).filter(Boolean).length);
