import {printCoordMap, readCoordMapAndBounds} from "../lib/read.js";
import {pairs} from "../lib/utils.js";

function calculateAntinodes([[x1, y1], [x2, y2]]) {
    return [[x1-(x2-x1), y1-(y2-y1)], [x2+(x2-x1), y2+(y2-y1)]]
}

// const [input, [height, width]] = readCoordMapAndBounds("test1.txt")
const [input, [height, width]] = readCoordMapAndBounds("input.txt")

const valid = ([x, y]) => x >= 0 && x < width && y >= 0 && y < height;

printCoordMap(input, (c) => c);

const antennas = [...input.entries()].filter(([coords, symbol]) => symbol !== '.');

const groups = antennas.reduce((groups, [coords, antenna]) => {
    const a = groups.get(antenna) || [];
    a.push(coords);
    groups.set(antenna, a);
    return groups;
}, new Map())

const antinodes = groups.values()
    .map((antennas) => pairs(antennas))
    .map((group) => group.map((pairs) => calculateAntinodes(pairs).filter(valid)));

        // pairs.map((pair) => [...pair, ...calculateAntinodes(pair)])))

const validAntinodes = [...antinodes].flat().flat();

const uniqueAntinodes = new Set(validAntinodes.map(([x, y])=> `${x}:${y}`))

// Paint the map and count if we can have an antinode there
// const result = validAntinodes.reduce((sum, coords) => {
//     if (input.get(coords) !== '#') {
//         input.set(coords, '#');
//         // console.log(coords);
//         // printCoordMap(input, c => c)
//         return sum + 1;
//     }
//     return sum;
// }, 0)
//
// printCoordMap(input, (c) => c);

console.log(uniqueAntinodes.size);
