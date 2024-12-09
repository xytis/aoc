import {readCoordMap} from "../lib/read.js";
import {check, next} from "../lib/coords.js";

// const input = readCoordMap("test2.txt")
const input = readCoordMap("input.txt")

const MAS = ['M', 'A', 'S'];
function hasXmas(coords) {
    const rb = check(next(coords, 7), 3, MAS, input);
    const lt = check(next(coords, 3), 7, MAS, input);

    const rt = check(next(coords, 5), 1, MAS, input);
    const lb = check(next(coords, 1), 5, MAS, input);

    return (rb || lt) && (rt || lb);
}

const matches = [...input.keys()].map(hasXmas).reduce((a, b) => a + b);
console.log(matches);
