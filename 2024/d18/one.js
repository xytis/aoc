import {printAnyGrid, read} from "../lib/read.js";
import ArrayKeyedMap from "array-keyed-map";
import {next} from "../lib/coords.js";
import { Heap } from 'heap-js';

// const input = read("test.txt");
// const H = 7;
// const W = 7;
// const T = 12;

const input = read("input.txt");
const H = 71;
const W = 71;
const T = 1024;

const order = input.split("\n")
    .reduce((acc, e, i) => {
        const coords = e.split(",").map(e => parseInt(e, 10));
        acc.set(coords, i+1);
        return acc;
    }, new ArrayKeyedMap()) ;

function inBounds(coords) {
    return coords[0] >= 0 && coords[0] < W && coords[1] >= 0 && coords[1] < H;
}

printAnyGrid((n) => {
    if (!inBounds(n)) {
        return undefined;
    }
    if (order.get(n) <= T) {
        return '#';
    }
    return '.';
})

const start = [0,0];
const end = [W-1, H-1];

function byDistance(a, b) {
    return b[1] - a[1];
}

function neighbours(coords, time) {
    return [0, 2, 4, 6].map(d => {
        const n = next(coords, d);
        if (!inBounds(n)) {
            return undefined;
        }
        if (order.get(n) <= T) {
            return undefined;
        }
        return [n, time + 1];
    }).filter(Boolean);
}

console.log(order.get(end))

const unvisited = new Heap(byDistance);
unvisited.push([start, 0]);

const visited = new ArrayKeyedMap();

let iter = 0;

while (unvisited.length > 0) {
    iter++;
    const current = unvisited.pop();
    const [coords, cost] = current;
    visited.set(coords, cost);
    for (const neighbour of neighbours(coords, cost)) {
        const [coords, cost] = neighbour;
        if (visited.has(coords)) {
            const previous = visited.get(coords);
            if (previous > cost) {
                // Neighbour is viable
                unvisited.push(neighbour);
            }
        } else {
            // Neighbour was not visited
            unvisited.push(neighbour);
        }
    }

    // if (iter % 1000 === 0) {
    //     printAnyGrid((n) => {
    //         if (!inBounds(n)) {
    //             return undefined;
    //         }
    //         if (order.get(n) <= T) {
    //             return '#';
    //         }
    //         if (visited.has(n)) {
    //             return visited.get(n).toString().slice(-1);
    //         }
    //         return '.';
    //     })
    //
    //     // console.log(unvisited.slice(0, 10));
    // }
}

console.log(visited.get(end));
