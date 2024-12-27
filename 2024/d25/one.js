import {read} from "../lib/read.js";

const schematics = read("input.txt").split('\n\n').map(s => s.trim());

const locks = [];
const keys = [];

const WIDTH = 5;
const HEIGHT = 7;

schematics.forEach(schematic => {
    const lines = schematic.split('\n');
    if (lines[0][0] === '#') {
        const lock = [];
        for (let pin = 0; pin < WIDTH; pin++) {
            for (let height = 0; lines[height][pin] === '#'; height++) {
                lock[pin] = height;
            }
        }
        locks.push(lock);
    }
    if (lines[0][0] === '.') {
        const key = [];
        for (let pin = 0; pin < WIDTH; pin++) {
            for (let height = HEIGHT - 1; lines[height][pin] === '#'; height--) {
                key[pin] = HEIGHT - height - 1;
            }
        }
        keys.push(key);
    }
})

function fits(lock, key) {
    for (let pin = 0; pin < WIDTH; pin++) {
        if (lock[pin] + key[pin] > HEIGHT - 2) {
            return false;
        }
    }
    return true;
}

const combinations = locks.flatMap(d => keys.map(v => [d, v]))
const res = combinations.map(([lock, key]) => fits(lock, key));

console.log(res.filter(Boolean).length);
