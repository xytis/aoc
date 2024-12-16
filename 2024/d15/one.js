import {printCoordMap, read} from "../lib/read.js";
import ArrayKeyedMap from "array-keyed-map";
import {next} from "../lib/coords.js";

const [mapIn, movesIn] = read("input.txt").split("\n\n");

let rx = 0;
let ry = 0;

const lines = mapIn.trim().split("\n");
const symbols = lines.map(l => l.split(''))
const map = new ArrayKeyedMap()
for (let y = 0, H = symbols.length; y < H; y++) {
    for (let x = 0, W = symbols[y].length; x < W; x++) {
        map.set([x, y], symbols[y][x])
        if (symbols[y][x] === '@') {
            rx = x;
            ry = y;
        }
    }
}

const moves = movesIn.trim().split("").map(d => {
    switch (d) {
        case '^': return 0;
        case '>': return 2;
        case 'v': return 4;
        case '<': return 6;
    }
}).filter(d => d!== undefined);

const move = (dir) => {
    let stack = 0;
    let blocked = false;
    let now = [rx, ry]
    while (true) {
        // We need to check if there is an empty location in this direction
        const at = next(now, dir)
        if (map.get(at) === '.') {
            now = at;
            break;
        }
        if (map.get(at) === '#') {
            blocked = true;
            break;
        }
        stack++;
        now = at;
    }
    if (blocked) {
        // no movement
        return;
    }
    // move the stack
    map.set([rx, ry], '.');
    const [nx, ny] = next([rx, ry], dir)
    const w = map.get([nx, ny]); // this is either '.' or 'O'
    map.set(now, w); // this "shifts" the stack
    map.set([nx, ny], '@'); // this returns the robot back
    rx = nx
    ry = ny
}


// printCoordMap(map, c=>c)
// console.log(rx, ry)
// console.log(moves);

moves.forEach(step => {
    move(step);
    // printCoordMap(map, c=>c)
})

// Count the boxes:

const res = [...map.entries()].reduce((a, [[x, y], v]) => {
    if (v === 'O') {
        a = a + y * 100 + x;
    }
    return a;
}, 0)

console.log(res)
