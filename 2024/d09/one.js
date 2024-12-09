import {read} from "../lib/read.js";

// const input = read('test.txt').trim();
const input = read('input.txt').trim();
const EMPTY = '.';

const unpacked = [];

let index = 0;
let file = true;
for (let i = 0; i < input.length; i++) {
    const block = parseInt(input[i], 10);
    for (let j = 0; j < block; j++) {
        if (file) {
            unpacked.push(index);
        } else {
            unpacked.push(EMPTY);
        }
    }
    if (file) {
        index = index + 1;
    }
    file = !file;
}

console.log(unpacked);

const sorted = [...unpacked];
let head = 0;
let tail = unpacked.length - 1;
while (head < tail) {
    if (sorted[head] === EMPTY) {
        sorted[head] = sorted[tail];
        sorted[tail] = EMPTY;
        head = head + 1;
        tail = tail - 1;
        while (sorted[tail] === EMPTY) {
            tail = tail - 1;
        }
    } else {
        head = head + 1;
    }
}

console.log(sorted);
const res = sorted.reduce((acc, val, index) => val !== EMPTY ? acc + index * val : acc, 0)
console.log(res);
