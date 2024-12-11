import {readCoordMap} from "../lib/read.js";
import {next} from "../lib/coords.js";
import ArrayKeyedMap from "array-keyed-map";

const input = readCoordMap("input.txt")
// const input = readCoordMap("test2.txt")

const starts = [...input.entries()].filter(([coords, height]) => height === '0');

function validUphillNeighbours(coords, target) {
    return [
        next(coords, 0),
        next(coords, 2),
        next(coords, 4),
        next(coords, 6),
        ].map((coords) => [coords, input.get(coords)])
        .filter(([coords, height]) => height === `${target}`);
}

const points = starts.map((start) => {
    let branches = [start];
    for (let t = 1; t < 10; t++) {
        const p = [];
        for (let [coords, _] of branches) {
            p.push(...validUphillNeighbours(coords, t));
        }
        branches = p;
    }
    return branches.length;
}).reduce((acc, p) => acc + p, 0);



console.log(points);
