import {printCoordMap, readCoordMap} from "../lib/read.js";
import {next, turnRight} from "../lib/coords.js";
import ArrayKeyedMap from "array-keyed-map";

// const input = readCoordMap("test1.txt")
// const input = readCoordMap("input.txt")
const input = readCoordMap("testmax.txt")

const WALL = '#'
const OBSTRUCTION = 'O'
const EMPTY = '.'

// 7 0 1
// 6 X 2
// 5 4 3

function pathPrinter([char, ...history]) {
    let h = false;
    let v = false;
    if (history.includes(0) || history.includes(4)) {
        v = true;
    }
    if (history.includes(6) || history.includes(2)) {
        h = true;
    }
    if (h && v) {
        return '+';
    }
    if (h) {
        return '-';
    }
    if (v) {
        return '|';
    }
    return char;
}

function checkForLoops(dir, coords, m) {
    // console.log("c:", coords);
    // printCoordMap(m, pathPrinter);
    // peek:
    const [now, ...history] = m.get(coords)
    const step = next(coords, dir)
    const [ahead] = m.get(step) || [];
    // act:
    switch (ahead) {
        case undefined:
            // Exit the map, no loops
            history.push(dir)
            m.set(coords, [now, ...history])
            return false;
        case WALL:
        case OBSTRUCTION:
            // check if history contains current traveling direction
            if (history.includes(dir)) {
                return true;
            }
            history.push(dir)
            m.set(coords, [now, ...history])
            return checkForLoops(turnRight(dir), coords, m);
        case EMPTY:
            history.push(dir)
            m.set(coords, [now, ...history])
            return checkForLoops(dir, step, m);
    }
}

let length = 0;

function goToExit(dir, coords, resolve, loops, m) {
    // console.log("e:", coords);
    // printCoordMap(m, pathPrinter);
    // peek:
    const [now, ...history] = m.get(coords)
    const step = next(coords, dir)
    const [ahead, ...touched] = m.get(step) || [];
    length = length + 1;
    // act:
    switch (ahead) {
        case undefined:
            // Exit the map
            history.push(dir)
            m.set(coords, [now, ...history])
            console.log("path", length)
            return resolve(loops);
        case WALL:
        case OBSTRUCTION:
            history.push(dir)
            m.set(coords, [now, ...history])
            return goToExit(turnRight(dir), coords, resolve, loops, m);
        case EMPTY:
            // We can only place virtual obstruction on a path that was not touched before
            if (touched.length === 0) {
                const copy = clone(m)
                copy.set(step, [OBSTRUCTION])
                // console.log("o:", step);
                // printCoordMap(copy, pathPrinter);
                const loop = checkForLoops(dir, coords, copy);
                if (loop) {
                    // console.log(`l ${loops.size}:`, step);
                    // if (loops.size % 100 === 0) {
                    // printCoordMap(copy, pathPrinter);
                    // }
                    loops.add(step.join(':'));
                }
            }
            history.push(dir)
            m.set(coords, [now, ...history])
            setTimeout(() => {
                goToExit(dir, step, resolve, loops, m)
            });
    }
}

let dir = '';
let start = [];

input.forEach((field, coords) => {

})

const empty = new ArrayKeyedMap()

input.forEach((field, coords) => {
    switch (field) {
        case '^':
            dir = 0;
            start = coords;
            empty.set(coords, [EMPTY]);
            return;
        default:
            empty.set(coords, [field]);
            return;
    }
})

function clone(i) {
    const c = new ArrayKeyedMap();
    i.forEach((v, k) => {
        c.set(k, v);
    });
    return c;
}

// console.log("s:");
// printCoordMap(empty, pathPrinter);

goToExit(dir, start, function (loops) {
    console.log(loops.size);
}, new Set(), empty)
