import {range, take} from "../lib/utils.js";
import {readLines} from "../lib/read.js";

function mix(num, value) {
    return value ^ num;
}

function prune(num) {
    return num % 16777216n;
}

function div(num, value) {
    return num / value;
}

function mul(num, value) {
    return num * value;
}

function generate(num) {
    num = prune(mix(num, mul(num, 64n)));
    num = prune(mix(num, div(num, 32n)));
    num = prune(mix(num, mul(num, 2048n)));
    return num;
}

function* prices(seed) {
    let cur = seed;
    let remaining = 2001;
    while (remaining > 0) {
        yield cur;
        remaining--;
        cur = generate(cur);
    }
}

function n2000(start) {
    const arr = range(2001);
    arr[0] = start;
    for (let i = 1; i < arr.length; i++) {
        arr[i] = generate(arr[i - 1]);
    }
    return arr;
}

function lastDigit(big) {
    let digit = big % 10n;
    return Number(digit);
}

const totals = new Map();

function populate(generator) {
    const array = [...generator]
    const first = new Map();
    // We start with 5th element in the sequence, because we need 4 diffs.
    for (let i = 4; i < array.length; i++) {
        const d0 = array[i - 3] - array[i - 4];
        const d1 = array[i - 2] - array[i - 3];
        const d2 = array[i - 1] - array[i - 2];
        const d3 = array[i] - array[i - 1];
        const key = `${d0}:${d1}:${d2}:${d3}`;
        const value = array[i];
        if (first.has(key)) {
            continue;
        }
        first.set(key, value)
    }
    // Merge first occurs to totals
    for (let key of first.keys()) {
        // const value = first.get(key);
        // if (totals.has(key)) {
        //     let sum = totals.get(key);
        //     sum = sum + value;
        //     totals.set(key, sum);
        // } else {
        //     totals.set(key, value)
        // }
        totals.set(key, (totals.get(key) || 0) + first.get(key));
    }
    return first;
}

const input = readLines("input-kk.txt").map(x => BigInt(x.trim()));

const firsts = input
    .map(x => prices(x).map(lastDigit))
    .map(populate);
// console.log(firsts);

const [best, value] = [...totals.entries()].reduce(
    ([at, max], [key, value]) => {
        if (value >= max) {
            return [key, value];
        }
        return [at, max];
    },
    ["", 0]
);

// for (const [i, first] of firsts.entries()) {
//     if (first.has(best)) {
//         console.log(i, first.get(best));
//     }
// }

console.log(best, value);
