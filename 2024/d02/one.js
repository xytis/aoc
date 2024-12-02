import {readLines} from "../lib/read.js";

const lines = readLines('./input.txt');
// const lines = readLines('./test.txt');

const reports = lines.map(l => l.split(/\s+/).map(s => parseInt(s, 10)));

function pairwise(arr, func){
    for(let i=0; i < arr.length - 1; i++){
        func(arr[i], arr[i + 1])
    }
}

function withinp(n) {
    return (n>=1 && n<= 3);}
function withinn(n) {
    return (n>=-3 && n<= -1);
}

const safe = reports.map(r => {
    const diff = []
    pairwise(r, (a, b) => diff.push(a - b))
    const [_, safe] = diff.reduce((acc, n) => {
        const [dir, safe] = acc;
        if (!safe) return acc;
        switch (dir) {
            case '?':
                return [n > 0 ? '+' : '-', withinp(n) || withinn(n)];
            case '-':
                return [dir, withinn(n)];
            case '+':
                return [dir, withinp(n)];
        }
        return acc;
    }, ['?', true])
    return safe
})

const sum = safe.reduce((acc, n) => {
    if (n) return acc + 1;
    return acc;
}, 0)

console.log(sum)
