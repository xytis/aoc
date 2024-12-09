import {readCoordMap} from "../lib/read.js";
import {next} from "../lib/coords.js";

// const input = readCoordMap("test1.txt")
const input = readCoordMap("input.txt")

const WALL = '#'
const TRAIL = 'o'
const EMPTY = '.'

// 7 0 1
// 6 X 2
// 5 4 3

function turnRight(dir) {
    return (dir + 2) % 8;
}

function behave(dir, coords, m) {
    // peek:
    const step = next(coords, dir)
    const ahead = m.get(step);
    // act:
    switch (ahead) {
        case undefined:
            // Exit the map
            m.set(coords, TRAIL)
            return m;
        case WALL:
            m.set(coords, TRAIL)
            return behave(turnRight(dir), coords, m);
        case TRAIL:
        case EMPTY:
            m.set(coords, TRAIL)
            return behave(dir, step, m);
    }
}

let dir = '';
let start = [];

input.forEach((field, coords) => {
    switch (field) {
        case '^':
            dir = 0;
            start = coords;
            input.set(coords, TRAIL);
            return;
    }
})

// console.log(dir)
// console.log(start)

const m = behave(dir, start, input)

let sum = 0;
input.forEach((field, _) => {
    if (field === TRAIL) {
        sum = sum + 1;
    }
})

console.log(sum)
