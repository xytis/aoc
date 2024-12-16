import {printCoordMap, read} from "../lib/read.js";
import ArrayKeyedMap from "array-keyed-map";
import {next} from "../lib/coords.js";

const [mapIn, movesIn] = read("input.txt").split("\n\n");

if (!Array.prototype.last) {
    Array.prototype.last = function () {
        return this[this.length - 1];
    };
}
;

let rx = 0;
let ry = 0;

const lines = mapIn.trim().split("\n");
const symbols = lines.map(l => l.split(''))
const map = new ArrayKeyedMap()
for (let y = 0, H = symbols.length; y < H; y++) {
    for (let x = 0, W = symbols[y].length; x < W; x++) {
        const sym = symbols[y][x];
        switch (sym) {
            case '#':
                map.set([x * 2, y], '#')
                map.set([x * 2 + 1, y], '#')
                break;
            case 'O':
                map.set([x * 2, y], '[')
                map.set([x * 2 + 1, y], ']')
                break;
            case '.':
                map.set([x * 2, y], '.')
                map.set([x * 2 + 1, y], '.')
                break;
            case '@':
                rx = x * 2;
                ry = y;
                map.set([x * 2, y], '@')
                map.set([x * 2 + 1, y], '.')
                break;
        }
    }
}

const moves = movesIn.trim().split("").map(d => {
    switch (d) {
        case '^':
            return 0;
        case '>':
            return 2;
        case 'v':
            return 4;
        case '<':
            return 6;
    }
}).filter(d => d !== undefined);

const moveH = (dir) => {
    if (dir !== 2 && dir !== 6) {
        return;
    }
    let stack = [[rx, ry]];
    let blocked = false;
    let cont = true
    while (cont) {
        // We need to check if there is an empty location in this direction
        const coords = stack.last();
        const at = next(coords, dir)
        const thing = map.get(at);
        cont = false;
        switch (thing) {
            case '#':
                blocked = true;
                break;
            case '[':
            case ']':
                stack.push(at);
                cont = true;
                break;
        }
    }
    if (blocked) {
        // no movement
        return;
    }
    // move the stack
    for (let coords of stack.reverse()) {
        const to = next(coords, dir)
        if (map.get(to) !== '.') {
            debugger;
        }
        map.set(to, map.get(coords));
        map.set(coords, '.');
    }
    // move the bot coords
    const s = next([rx, ry], dir);
    rx = s[0]
    ry = s[1]
}

const moveV = (dir) => {
    if (dir !== 0 && dir !== 4) {
        return;
    }
    let stack = [[[rx, ry]]];
    let blocked = false;
    while (true) {
        // we need to discover if there are items in the direction scouting by the last "front" of the stack.
        const front = stack.last();
        const n = new ArrayKeyedMap();
        for (let coords of front) {
            const at = next(coords, dir)
            const thing = map.get(at);
            switch (thing) {
                case '#':
                    blocked = true;
                    break;
                case '[':
                    n.set(at, true);
                    n.set(next(at, 2), true)
                    break;
                case ']':
                    n.set(at, true);
                    n.set(next(at, 6), true)
                    break;
            }
        }
        if (n.size === 0) {
            break;
        }
        if (blocked) {
            break;
        }
        stack.push([...n.keys()]);
    }
    if (blocked) {
        // no movement
        return;
    }
    // move the stack
    for (const front of stack.reverse()) {
        for (let coords of front) {
            const to = next(coords, dir)
            if (map.get(to) !== '.') {
                debugger;
            }
            map.set(to, map.get(coords));
            map.set(coords, '.');
        }
    }
    // move the bot coords
    const s = next([rx, ry], dir);
    rx = s[0]
    ry = s[1]
}


// printCoordMap(map, c => c)
// console.log(rx, ry)
// console.log(moves);

moves.forEach(step => {
    // console.log(step);
    moveH(step);
    moveV(step);
    // printCoordMap(map, c => c)
})

// Count the boxes:

const res = [...map.entries()].reduce((a, [[x, y], v]) => {
    if (v === '[') {
        a = a + y * 100 + x;
    }
    return a;
}, 0)

// printCoordMap(map, c => c)

console.log(res)

