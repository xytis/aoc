import {readLines} from "../lib/read.js";
import ArrayKeyedMap from "array-keyed-map";
import * as path from "node:path";

function panic(x, y, board) {
    return (board['E'][0] === x && board['E'][1] === y);
}

function translateV(sx, sy, ex, ey, board) {
    const path = [];
    let nx = sx, ny = sy;
    // First vertical
    if (ey < sy) {
        for (let y = ey; y < sy; y++) {
            ny = ny - 1;
            path.push('^')
            if (panic(nx, ny, board)) return undefined;
        }
    }
    if (ey > sy) {
        for (let y = ey; y > sy; y--) {
            ny = ny + 1;
            path.push('v')
            if (panic(nx, ny, board)) return undefined;
        }
    }
    // Then horizontal
    if (ex > sx) {
        for (let x = ex; x > sx; x--) {
            nx = nx + 1;
            path.push('>')
            if (panic(nx, ny, board)) return undefined;
        }
    }
    if (ex < sx) {
        for (let x = ex; x < sx; x++) {
            nx = nx - 1;
            path.push('<')
            if (panic(nx, ny, board)) return undefined;
        }
    }
    return path.join('');
}

function translateH(sx, sy, ex, ey, board) {
    const path = [];
    let nx = sx, ny = sy;
    // First horizontal
    if (ex > sx) {
        for (let x = ex; x > sx; x--) {
            nx = nx + 1;
            path.push('>')
            if (panic(nx, ny, board)) return undefined;
        }
    }
    if (ex < sx) {
        for (let x = ex; x < sx; x++) {
            nx = nx - 1;
            path.push('<')
            if (panic(nx, ny, board)) return undefined;
        }
    }
    // Then vertical
    if (ey < sy) {
        for (let y = ey; y < sy; y++) {
            ny = ny - 1;
            path.push('^')
            if (panic(nx, ny, board)) return undefined;
        }
    }
    if (ey > sy) {
        for (let y = ey; y > sy; y--) {
            ny = ny + 1;
            path.push('v')
            if (panic(nx, ny, board)) return undefined;
        }
    }
    return path.join('');
}

/*
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
  E | 0 | A |
    +---+---+
 */
const numericKeypad = {
    '7': [0, 0],
    '8': [1, 0],
    '9': [2, 0],
    '4': [0, 1],
    '5': [1, 1],
    '6': [2, 1],
    '1': [0, 2],
    '2': [1, 2],
    '3': [2, 2],
    'E': [0, 3],
    '0': [1, 3],
    'A': [2, 3],
}
const numericActions = new ArrayKeyedMap([
    [[0, 0], '7'],
    [[1, 0], '8'],
    [[2, 0], '9'],
    [[0, 1], '4'],
    [[1, 1], '5'],
    [[2, 1], '6'],
    [[0, 2], '1'],
    [[1, 2], '2'],
    [[2, 2], '3'],
    [[0, 3], 'E'],
    [[1, 3], '0'],
    [[2, 3], 'A'],
]);

function paths(start, end, board) {
    const [sx, sy] = board[start];
    const [ex, ey] = board[end];
    const v = translateV(sx, sy, ex, ey, board);
    const h = translateH(sx, sy, ex, ey, board);
    if (v && h && v !== h) {
        return [v, h];
    }
    if (v) {
        return [v];
    }
    return [h];
}

// function inputs(board, sequence) {
//     if (sequence instanceof Array) {
//         const steps = [];
//         for (let step of sequence) {
//             steps.push(inputs(board, step))
//         }
//         return steps;
//     }
//     const steps = [];
//     let position = 'A'; // Reset on each step
//     for (let i = 0; i < sequence.length; i++) {
//         const step = [];
//         for (let path of paths(position, sequence[i], board)) {
//             step.push(path + 'A');
//         }
//         steps.push(step);
//         position = sequence[i];
//
//     }
//     return steps;
// }
//
// function output(board, actions, sequence) {
//     if (sequence instanceof Array) {
//         const out = [];
//         for (let step of sequence) {
//             out.push(output(board, actions, step))
//         }
//         return out;
//     }
//     const out = [];
//     let [x, y] = board['A'];
//     for (let i = 0; i < sequence.length; i++) {
//         switch (sequence[i]) {
//             case 'A':
//                 out.push(actions.get([x, y]));
//                 break;
//             case '^':
//                 y = y - 1;
//                 break;
//             case '<':
//                 x = x - 1;
//                 break;
//             case '>':
//                 x = x + 1;
//                 break;
//             case 'v':
//                 y = y + 1;
//                 break;
//         }
//     }
//     return out;
// }


/*
    +---+---+
  E | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+
 */
const arrowKeypad = {
    'E': [0, 0],
    '^': [1, 0],
    'A': [2, 0],
    '<': [0, 1],
    'v': [1, 1],
    '>': [2, 1],
}
const arrowActions = new ArrayKeyedMap([
    [[0, 0], 'E'],
    [[1, 0], '^'],
    [[2, 0], 'A'],
    [[0, 1], '<'],
    [[1, 1], 'v'],
    [[2, 1], '>'],
])



function human() {
    return function (outcome, target) {
        // console.log(`human inputs ${outcome} to get ${target}`);
        // Human can simply press buttons
        return outcome.length;
    }
}

function robot(name, board, next) {
    return function (outcome, target) {
        let sum = 0;
        let position = 'A';
        for (let i = 0; i < outcome.length; i++) {
            let min = Infinity;
            for (let path of paths(position, outcome[i], board)) {
                // console.log(`robot:${name} requests ${path + 'A'} to input ${outcome[i]} to get ${target}`);
                min = Math.min(next(path + 'A', outcome[i]), min);
            }
            // console.log(`robot:${name} receives input ${outcome[i]} with cost ${min}`);
            position = outcome[i];
            sum += min;
        }
        return sum;
    }
}

// const chain = robot(numericKeypad, human());
// console.log(chain('029', '029A'));
const chain = robot("0", numericKeypad, robot("1", arrowKeypad, robot("2", arrowKeypad, human())));
// console.log(chain('029A', "doors to open"));


// console.log(inputs('029A', numericKeypad));
// console.log(inputs(['<A'], arrowKeypad));
// console.log(inputs(['^^>A', '>^^A'], arrowKeypad));
// console.log(inputs(['^^>A', '>^^A'], arrowKeypad));

const input = readLines("input.txt").map(line => {
    return [line, parseInt(line.slice(0, -1))];
})

// console.log(input);

const results = input.map(([sequence, multiplier]) => {
    const cost = chain(sequence, `doors ${sequence} to open`);
    return [sequence, cost, cost * multiplier];
})

console.log(results);

console.log(results.reduce((acc, [a, b, c]) => acc + c, 0));
