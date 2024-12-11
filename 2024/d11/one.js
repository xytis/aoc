import {read} from "../lib/read.js";

// const input = read("test.txt").trim().split(' ').map(e => parseInt(e, 10));
const input = read("input.txt").trim().split(' ').map(e => parseInt(e, 10));


let current = input;
for (let i = 0; i < 25; i++) {
    for (let s = 0; s < current.length; s++) {
        if (current[s] === 0) {
            current[s] = 1;
            continue;
        }
        let t = `${current[s]}`;
        if (t.length % 2 === 0) {
            const a = t.slice(0, t.length/2);
            const b = t.slice(t.length/2);
            current[s] = [parseInt(a, 10), parseInt(b, 10)];
            continue;
        }
        current[s] = 2024 * current[s];
    }
    current = current.flat();
    // console.log(i+1, current);
}

console.log(current.length);
