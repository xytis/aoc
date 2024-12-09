import {read} from "../lib/read.js";

// const input = read("./test2.txt")
const input = read("./input.txt")

const scan = /(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don't\(\))/g;
const extract = /^mul\((\d{1,3}),(\d{1,3})\)$/;

const matches = input.match(scan);

const [filtered, _] = matches
    .reduce(([res, on], cur) => {
        if (on && cur === "don't()") {
            return [res, false];
        }
        if (!on && cur === "do()") {
            return [res, true];
        }
        if (cur === "do()" || cur === "don't()") {
            return [res, on];
        }
        if (on) {
            return [[...res, cur], on];
        } else {
            return [res, on];
        }
    }, [[], true]);

// console.log(filtered);

const sum = filtered
    .map(expr => extract.exec(expr))
    .map(([_, a, b]) => parseInt(a, 10) * parseInt(b, 10))
    .reduce((a, b) => a + b, 0);

console.log(sum);
