import {readCoordMap} from "../lib/read.js";
import {check} from "../lib/coords.js";
import {range} from "../lib/utils.js";

// const input = readCoordMap("test1.txt")
// const input = readCoordMap("test2.txt")
const input = readCoordMap("input.txt")

const XMAS = ['X', 'M', 'A', 'S'];
const matches = [...input.keys()].map((coords) => {
    return range(8).reduce((sum, dir) => {
        if (check(coords, dir, XMAS, input)) {
            return sum + 1;
        }
        return sum;
    }, 0);
}).reduce((a, b) => a + b);


console.log(matches);
