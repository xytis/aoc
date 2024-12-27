import {read} from "../lib/read.js";
import {memo, range, take} from "../lib/utils.js";
import * as test from "node:test";

const WIDTH = 45;

const wires = new Map();
const gates = new Map();
const switches = new Map();


const maxBits = new Map();
const tested = new Map();

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
        case 'AND':
            return Boolean(a & b);
        case 'OR':
            return Boolean(a | b);
        case 'XOR':
            return Boolean(a ^ b);
    }
}

function demulate(op, res) {
    switch (op) {
        case 'AND':
            switch (res) {
                case true:
                    return [[true, true]];
                case false:
                    return [[false, false], [true, false], [false, true]];
            }
            break;
        case 'OR':
            switch (res) {
                case true:
                    return [[false, true], [true, false], [true, true]];
                case false:
                    return [[true, true]];
            }
            break;
        case 'XOR':
            switch (res) {
                case true:
                    return [[false, true], [true, false]];
                case false:
                    return [[false, false], [true, true]];
            }
    }
}

const input = read("input.txt")
const [w, g] = input.split("\n\n").map(x => x.trim());
w.split('\n').forEach(parseWire);
g.split('\n').forEach(parseGate);

// All of this is manual work:
// # node two.js | dot -Tsvg >! output.svg
switches.set("qjj", "gjc")
switches.set("gjc", "qjj")
switches.set("gvm", "z26")
switches.set("z26", "gvm")
switches.set("wmp", "z17")
switches.set("z17", "wmp")
switches.set("qsb", "z39")
switches.set("z39", "qsb")

// Manual work for test case
// switches.set("z05", "z00")
// switches.set("z00", "z05")
// switches.set("z01", "z02")
// switches.set("z02", "z01")

// console.log(wires);
// console.log(gates);

function intoDigraph() {
    console.log("digraph G {")
    {
        console.log("subgraph cluster_x {")
        const wires = range(WIDTH).map(i => `x${i.toString(10).padStart(2, '0')}`);
        wires.forEach(wire => {
            console.log(`  w${wire} [shape=diamond]`)
        })
        take(wires, 2, 1).forEach(([a, b]) => {
            if (a && b) console.log(`  w${a} -> w${b} [style=dashed]`)
        });
        console.log("label = \"X\";")
        console.log("color=red;")
        console.log("}");
    }
    {
        console.log("subgraph cluster_y {")
        const wires = range(WIDTH).map(i => `y${i.toString(10).padStart(2, '0')}`);
        wires.forEach(wire => {
            console.log(`  w${wire} [shape=diamond]`)
        })
        take(wires, 2, 1).forEach(([a, b]) => {
            if (a && b) console.log(`  w${a} -> w${b} [style=dashed]`)
        });
        console.log("label = \"Y\";")
        console.log("color=blue;")
        console.log("}");
    }
    {
        console.log("subgraph cluster_z {")
        const wires = range(WIDTH).map(i => `z${i.toString(10).padStart(2, '0')}`);
        wires.forEach(wire => {
            console.log(`  w${wire} [shape=diamond]`)
        })
        take(wires, 2, 1).forEach(([a, b]) => {
            if (a && b) console.log(`  w${a} -> w${b} [style=dashed]`)
        });
        console.log("label = \"Z\";")
        console.log("color=green;")
        console.log("}");
    }
    for (let gate of gates.keys()) {
        let {op, a, b} = gates.get(gate);
        console.log(`  g${gate} [shape=box label="g:${gate}: ${a} ${op} ${b}"]`)
        console.log(`  w${a} -> g${gate}`);
        console.log(`  w${b} -> g${gate}`);
    }
    for (let gate of gates.keys()) {
        let wire = gate;
        if (switches.has(gate)) {
            wire = switches.get(wire);
        }
        console.log(`  w${wire} [shape=diamond ${tested.get(wire) ? 'color="red"' : ''}]`)
        console.log(`  g${gate} -> w${wire} [label="w:${wire}"]`);
    }
    console.log("}");
}


const output = memo(function (wire) {
    if (switches.has(wire)) {
        wire = switches.get(wire);
    }
    if (wires.has(wire)) {
        return wires.get(wire);
    }
    if (gates.has(wire)) {
        const {op, a, b} = gates.get(wire);
        return simulate(op, output(a), output(b));
    }
    return undefined;
})

const source = function (wire) {
    if (wires.has(wire)) {
        return {[wire]: wires.get(wire)};
    }
    if (gates.has(wire)) {
        const {op, a, b} = gates.get(wire);
        const t = tested.get(wire);
        return {[`${wire}:${op}:${t}:[${output(wire)}]`]: {...source(a), ...source(b)}};
    }
    return undefined;
}

const maxInput = memo(function (wire) {
    const m = (/^[xy](\d+)$/g).exec(wire);
    if (m) {
        return parseInt(m[1], 10);
    }
    if (gates.has(wire)) {
        const {_, a, b} = gates.get(wire);
        const ma = maxInput(a);
        const mb = maxInput(b);
        return Math.max(ma, mb);
    }
})

function assemble(prefix) {
    const bits = [];
    for (let i = 64; i >= 0; i--) {
        bits.push(output(`${prefix}${i.toString(10).padStart(2, '0')}`));
    }
    return parseInt(bits.filter(x => x !== undefined).map(Number).join(''), 2);
}

function sources(prefix) {
    const s = new Map();
    for (let i = 64; i >= 0; i--) {
        const wire = `${prefix}${i.toString(10).padStart(2, '0')}`
        if (source(wire) !== undefined) {
            s.set(wire, source(wire));
        }
    }
    return s;
}

const check = function (wire, value) {
    const current = output(wire);
    if (value !== current) {
        tested.set(wire, true);
    }
}

function set(prefix, value) {

    const bits = Number(value).toString(2).split('').reverse();
    for (let i = 0; i < WIDTH; i++) {
        const wire = `${prefix}${i.toString(10).padStart(2, '0')}`
        wires.set(wire, bits[i] === '1');
    }
}


// const z = assemble('z');
// const diff = z - (x + y);
//
// const s = sources('z')

for (const gate of gates.keys()) {
    maxBits.set(gate, maxInput(gate));
}

const ones = parseInt('1'.repeat(WIDTH + 1), 2);

const x = assemble('x');
const y = assemble('y');

wires.clear();
set('x', x);
set('y', y);
const o = assemble('z');
const z = (x + y);

const p = z.toString(2).split('').reverse().map(x => x === '1');
for (let i = 0; i < WIDTH; i++) {
    const out = `z${i.toString(10).padStart(2, '0')}`
    check(out, p[i]);
}

intoDigraph();

output.clear();
console.error("x:", x.toString(2).padStart(50, '0'));
console.error("y:", y.toString(2).padStart(50, '0'))
console.error("o:", o.toString(2).padStart(50, '0'))
console.error("z:", z.toString(2).padStart(50, '0'))
