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

// console.log(patterns, max, targets);

const match = memo((remainder) => {
    if (remainder.length === 0) {
        return 1;
    }
    let count = 0;
    for (let len = Math.min(max, remainder.length); len > 0; len--) {
        const search = remainder.slice(0, len);
        if (patterns.has(search)) {
            const branch = remainder.slice(len);
            count = count + match(branch);
        }
    }
    return count;
})

console.log(targets.map((t, i) => {
    const m = match(t);
    console.log(i, t, m);
    return m;
}).reduce((a, b) => a + b, 0));

