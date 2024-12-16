import {read} from "../lib/read.js";

// const input = read("test.txt").trim().split(' ').map(e => parseInt(e, 10));
const input = read("input.txt").trim().split(' ').map(e => parseInt(e, 10));

const branches = new Map(input.map(e => [e, 1]));
let prev = branches;

for (let i = 0; i < 75; i++) {
    // console.log("iter", i, "size", prev.size, "max", [...prev.keys()].sort((a, b) => b-a)[0]);
    const curr = new Map();
    prev.forEach((count, number) => {
        if (number === 0) {
            // every existing 0 converts to 1
            curr.set(1, (curr.get(1) || 0) + count)
            return;
        }
        let t = `${number}`;
        if (t.length % 2 === 0) {
            // number splits and joins any existing instances
            const a = parseInt(t.slice(0, t.length/2), 10);
            const b = parseInt(t.slice(t.length/2), 10);
            curr.set(a, (curr.get(a) || 0) + count)
            curr.set(b, (curr.get(b) || 0) + count)
            return;
        }
        const m = 2024 * number;
        curr.set(m, (curr.get(m) || 0) + count);
    })
    prev = curr;
}

const result = [...prev.values()].reduce((a, b) => a + b, 0);
console.log(result);
