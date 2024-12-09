import {read} from "../lib/read.js";

// const input = read('test.txt').trim();
const input = read('input.txt').trim();
const EMPTY = '.';

function free(fs, scanFrom, scanTo) {
    let head = scanFrom;
    while (fs[head] !== EMPTY && head < scanTo) {
        head = head + 1;
    }
    return head;
}

function length(fs, match, from) {
    let cur = from;
    while (fs[cur] === match) {
        cur = cur + 1;
    }
    return cur - from;
}

function rlength(fs, match, from) {
    let cur = from-1;
    while (fs[cur] === match) {
        cur = cur - 1;
    }
    return from - cur - 1;
}

function move(fs, head, from, until) {
    let tail = from;
    while (tail < until) {
        fs[head] = fs[tail];
        fs[tail] = EMPTY;
        head = head + 1;
        tail = tail + 1;
    }
}

function defrag(fs, indexAt, fileFrom, fileTo) {
    if (indexAt >= fileFrom) {
        return;
    }
    const size = fileTo - fileFrom;
    //scan for free space
    let head = free(fs, indexAt, fileFrom);
    if (head === fileFrom) {
        // No free space was found
        return;
    }
    // Now check if this free chunk is large enough
    if (length(fs, EMPTY, head) >= size) {
        move(fs, head, fileFrom, fileTo);
        return;
    }
    // else, attempt to defrag to next chunk
    defrag(fs, head+1, fileFrom, fileTo);
}

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

const sorted = [...unpacked];
let head = 0;
let tail = sorted.length;
while (tail > 0) {
    while (sorted[head] !== EMPTY) {
        head = head+1;
    }
    while (sorted[tail-1] === EMPTY) {
        tail = tail-1;
    }
    const file = sorted[tail-1];
    const length = rlength(sorted, file, tail);
    defrag(sorted, head, tail-length, tail);
    tail = tail-length;
}

// console.log(sorted);
const res = sorted.reduce((acc, val, index) => val !== EMPTY ? acc + index * val : acc, 0)
console.log(res);
