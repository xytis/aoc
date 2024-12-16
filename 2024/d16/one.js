import ArrayKeyedMap from "array-keyed-map";
import {readLines} from "../lib/read.js";
import {next} from "../lib/coords.js";

const lines = readLines("input.txt")

let exit;
let start;

const symbols = lines.map(l => l.split(''))
const map = new ArrayKeyedMap()
for (let y = 0, H = symbols.length; y < H; y++) {
    for (let x = 0, W = symbols[y].length; x < W; x++) {
        if (symbols[y][x] === '.') {
            map.set([x, y], true)
        }
        if (symbols[y][x] === 'E') {
            exit = [x, y];
            map.set([x, y], true)
        }
        if (symbols[y][x] === 'S') {
            start = [x, y];
            map.set([x, y], true)
        }
    }
}

function byDistance(a, b) {
    return b[2] - a[2]; //sort by distance
}

function calculateNeighbours([dir, coords, cost]) {
    return [0, 2, 4, 6].map(d => {
        // Only viable step forward
        if (d === dir) {
            const n = next(coords, dir);
            if (map.has(n)) {
                return [d, n, cost + 1];
            }
            return undefined;
        }
        // Remaining turns are "neighbours" too.
        return [d, coords, cost + 1000];
    }).filter(Boolean); // Drop the maybe step.
}

const unvisited = [];
const visited = new ArrayKeyedMap();
unvisited.push([2, start, 0]); // Initial node is cost 0 East at start

while (unvisited.length > 0) {
    const current = unvisited.pop();
    const [dir, coords, cost] = current;
    const id = [dir, ...coords];
    if (visited.has(id)) {
        const prev = visited.get(id);
        if (prev < cost) {
            continue;
        }
    }
    visited.set(id, cost);
    const neighbours = calculateNeighbours(current);
    for (const neighbour of neighbours) {
        const [dir, coords, cost] = neighbour;
        const id = [dir, ...coords];
        if (visited.has(id)) {
            const previous = visited.get(id);
            if (previous > cost) {
                // Neighbour is viable
                unvisited.push(neighbour);
            }
        } else {
            // Neighbour was not visited
            unvisited.push(neighbour);
        }
    }
    unvisited.sort(byDistance);
    // console.log("VISITED", [...visited.entries()])
    // console.log("UNVISITED", unvisited);
}

const res = [0, 2, 4, 6].reduce((min, d) => {
    return Math.min(visited.get([d, ...exit]), min);
}, Infinity);

console.log(res);
