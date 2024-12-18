import {read} from "../lib/read.js";
import {gcd, lcm} from "../lib/math.js";

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
    B = B ^ C;
    SP = SP + 2;
}

function out(n) {
    output.push(combo(n) % 8);
    SP = SP + 2;
}

function bdv(n) {
    B = Math.floor(A / (Math.pow(2, combo(n))));
    SP = SP + 2;
}

function cdv(n) {
    C = Math.floor(A / (Math.pow(2, combo(n))));
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

// console.log(A, B, C, SP, program, output);

while (true) {
    const op = program[SP];
    const n = program[SP+1];
    if (op === undefined) break;
    opcode[op](n);
}

console.log(output.join(','));
