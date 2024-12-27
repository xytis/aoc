import {read} from "../lib/read.js";

const wires = new Map();
const gates = new Map();

function parseWire(line) {
    const [name, value] = line.split(':').map(x => x.trim())
    wires.set(name, value === '1');
}

function parseGate(line) {
    const [logic, output] = line.split('->').map(x => x.trim())
    const [a, op, b] = logic.split(' ').map(x => x.trim())
    gates.set(output, {op, a, b})
}

function simulate(op, a, b) {
    switch (op) {
        case 'AND': return a & b;
        case 'OR': return a | b;
        case 'XOR': return a ^ b;
    }
}

const input = read("input.txt")
const [w, g] = input.split("\n\n").map(x => x.trim());
w.split('\n').forEach(parseWire);
g.split('\n').forEach(parseGate);

// console.log(wires);
// console.log(gates);

const output = function (wire) {
    if (wires.has(wire)) {
        return wires.get(wire);
    }
    if (gates.has(wire)) {
        const {op, a, b} = gates.get(wire);
        return simulate(op, output(a), output(b));
    }
    return undefined;
}

function assemble() {
    const bits = [];
    for (let i = 64; i >= 0; i--) {
        bits.push(output(`z${i.toString(10).padStart(2, '0')}`));
    }
    return parseInt(bits.filter(x => x !== undefined).join(''), 2);
}

console.log(assemble());
