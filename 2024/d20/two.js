import {printAnyGrid, readCoordMap, scanCoordMap} from "../lib/read.js";
import ArrayKeyedMap from "array-keyed-map";
import {Heap} from "heap-js";
import {next} from "../lib/coords.js";

const DISTANCE = 100;
const [map, start, finish] = scanCoordMap("input.txt", (acc, coords, symbol) => {
    switch (symbol) {
        case "S":
            acc[0].set(coords, '.');
            acc[1] = coords;
            break;
        case "E":
            acc[0].set(coords, '.');
            acc[2] = coords;
            break;
        default:
            acc[0].set(coords, symbol);
            break;
    }
    return acc;
}, [new ArrayKeyedMap(), undefined, undefined]);

console.log("s", start);
console.log("e", finish);
printAnyGrid(c => {
    if (map.get(c) === '#') {
        return '#';
    }
    if (map.has(c)) {
        return ' ';
    }
});

function byDistance(a, b) {
    return b[1] - a[1];
}

function neighbours(coords, cost) {
    return [0, 2, 4, 6].map(d => {
        const n = next(coords, d);
        if (map.get(n) === '.') {
            return [n, cost + 1];
        }
    }).filter(Boolean);
}

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
}

function traverse(map, coords) {
    const path = [];
    while (true) {
        const cost = map.get(coords);
        const n = [0, 2, 4, 6].map(d => {
            const n = next(coords, d);
            return [n, map.get(n) === cost - 1];
        })
            .find(([_, y]) => y);
        if (n) {
            path.push(n[0]);
            coords = n[0];
        } else {
            break;
        }
    }
    return path.reverse();
}

// We now have a fully calculated map.
// Let's extract the path:
const path = traverse(visited, finish);

// console.log(start);
// console.log(path);
// console.log(finish);

// There are 41^2/2 possible slots and 10000 path nodes. Just iterate over all.

function distance(a, b) {
    return Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]);
}

const visitedByCost = [...visited.entries()].sort((a, b) => b[1] - a[1])

function glitch(s, cost) {
    const destinations = [];
    for (let i = 0, l = visitedByCost.length; i < l; ++i) {
        const [coords, value] = visitedByCost[i];
        if (value < cost) {
            // no more sense to check nodes that are further than we are.
            break;
        }
        const d = distance(coords, s);
        if (d > 20) {
            // this node is unreachable by glitch
            continue;
        }
        const saving = value - cost - d;
        if (saving >= DISTANCE) {
            destinations.push([coords, saving]);
        }
    }
    return destinations;
}

const savings = path.reduce((acc, c) => {
    const cost = visited.get(c);
    glitch(c, cost).forEach(([_, savings]) => {
        acc.set(savings, (acc.get(savings) || 0) + 1)
    })
    return acc;
}, new Map())

// console.log(savings);
const over100 = savings.entries()
    .reduce((acc, [_, count]) => {
        return acc + count;
    }, 0)
console.log(over100);
