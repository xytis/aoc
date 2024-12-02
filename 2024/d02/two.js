import {readLines} from "../lib/read.js";

const lines = readLines('./input.txt');
// const lines = readLines('./test.txt');

const reports = lines.map(l => l.split(/\s+/).map(s => parseInt(s, 10)));

function pairwise(arr, func){
    for(let i=0; i < arr.length - 1; i++){
        func(arr[i], arr[i + 1])
    }
}

function withinp(a, b) {
    if (a === '*' || b === '*') return true;
    const n = a-b;
    return (n>=1 && n<= 3);
}

function withinn(a, b) {
    if (a === '*' || b === '*') return true;
    const n = a-b;
    return (n>=-3 && n<= -1);
}

function match([a, b, c, ...rest], check, tolerance) {
    if (b === undefined) return true;
    let result = false;
    if (tolerance !== 0) {
        if (check(a, c)) {
            result = result || match([a, c, ...rest], check, tolerance-1);
        }
    }
    if (check(a, b)) {
        result = result || match([b, c, ...rest], check, tolerance);
    }
    return result;
}

const safe = reports.map(r => {

    const bounded = ['*', ...r, '*'];
    const p = match(bounded, withinp, 1);
    const n = match(bounded, withinn, 1);
    return n || p;
})

const sum = safe.reduce((acc, n) => {
    if (n) return acc + 1;
    return acc;
}, 0)

console.log(sum)
