import {read} from "../lib/read.js";
import {gcd} from "../lib/math.js";

const input = read('input.txt')

const [initStack, initProgram] = input.split('\n\n')

let [A, B, C] = initStack
    .split('\n')
    .map(line => line.split(':')[1].trim())
    .map(n => parseInt(n, 10));

const program = initProgram
    .split(':')[1].trim()
    .split(',')
    .map(n => parseInt(n, 10));

let SP = 0;

const output = [];

function literal(n) {
    return n;
}

function combo(n) {
    switch (n) {
        case 0:
        case 1:
        case 2:
        case 3:
            return n;
        case 4:
            return A;
        case 5:
            return B;
        case 6:
            return C;
        case 7:
            throw 'INVALID PROGRAM';
    }
}

function adv(n) {
    // Does not truncate
    A = Math.floor(A / (Math.pow(2, combo(n))));
    SP = SP + 2;
}

function bxl(n) {
    B = B ^ literal(n);
    SP = SP + 2;
}

function bst(n) {
    B = combo(n) % 8;
    SP = SP + 2;
}

function jnz(n) {
    if (A === 0) {
        SP = SP + 2;
        return;
    }
    SP = literal(n);
}

function bxc(n) {
    // Truncates
    B = (B ^ C) % 8;
    SP = SP + 2;
}

function out(n) {
    output.push(combo(n) % 8);
    SP = SP + 2;
}

function bdv(n) {
    // Truncates
    B = Math.floor(A / (Math.pow(2, combo(n))))%8;
    SP = SP + 2;
}

function cdv(n) {
    // Truncates
    C = (Math.floor(A / (Math.pow(2, combo(n)))))%8;
    SP = SP + 2;
}

const opcode = {
    0: adv,
    1: bxl,
    2: bst,
    3: jnz,
    4: bxc,
    5: out,
    6: bdv,
    7: cdv,
}

// 2,4, bst B=A%8       // take last 3 bits of A (cba)
// 1,1, bxl B=B^1       // take last bit (from cba) B = a
// 7,5, cdv C=A/(2^B)   // C = cba if a=0, dcb if a=1 (note that %8 is implied)
// 1,5, bxl B=B^5       // 5 = 101, so B = 10!a
// 4,5, bxc B=B^C       // a=0: 101 ^ cb0 = !cb1 a=1: 100 ^ dcb = !dc0
// 0,3, adv A=A/(2^3)   // remove last 3 bits (or last digit in base 3)
// 5,5, out B%8         // output last 3 bits of B (!cb1 or !dc0)
// 3,0, jnz A!=0

// input must be 16 digit in base 3 (so 8^16 or 2^48 as minimum)
// last 0 must be 000 (!dc0)
// 000 -> 4 (100)
// 1 -> 4
// if a = 4 (100) then (!dc0) = 000
// if a = 7 (111) then 3

// WAIT, we may work out in reverse. We need to find a number 0-7 which returns 0.
// Then we need to find another which returns 3, and so on.

// Oh, wait. Each output depends on 4 bits, and we remove 3 bits every iteration.

// console.log(A, B, C, SP, program, output);

function compute(a) {
    A = a;
    B = 0;
    C = 0;
    SP = 0;
    output.length = 0; // clear array.
    while (true) {
        // Debug trace:
        // console.log(
        //     "SP", SP,
        //     "A", A.toString(8).padStart(0,'0'),
        //     "B", B.toString(8).padStart(3,'0'),
        //     "C", C.toString(8).padStart(3,'0'),
        // )

        const op = program[SP];
        const n = program[SP + 1];
        if (op === undefined) break;
        opcode[op](n);
    }
}

// Because bits are impacting to the right, it makes more sense to build the number from the highs
// If we want the function to end in 3,0 then the high values should be 101???
// And since the 101 is no longer influenced by anything on the left, we are safe to take it.
// What we want to do is to create a tree, each with 8 leaves
// Imagine we have a number like this:
// ((0 + 4)*8 + 1)*8 + 7
// And we want this to result in [5, 3, 0]

function check(number, goal) {
    compute(number);
    for (let i = 0; i < goal.length; i++) {
        if (output[i] !== goal[i]) {
            return false;
        }
    }
    return true;
}

function branch(number, s, goal) {
    number = number * 8; // Shift left
    s = s + 1;
    const remainder = goal.slice(-s);
    for (let i = 0; i < 8; i++) {
        const target = number + i;
        // console.log("c:", target.toString(2),);
        if (check(target, remainder)) {
            console.log("m:", target, "t:", remainder.join(','));
            const delve = branch(target, s, goal);
            if (delve !== false) {
                return delve;
            }
        }
    }
    return false;
}

branch(0, 0, program);


// for (let a = 0; a < 8; a++) {
//     for (let b = 0; b < 8; b++) {
//         const num = parseInt(`${a}${b}`, 8)
//         compute(num);
//         console.log(num.toString(2).padStart(6, '0'), output.join(''));
//     }
// }
