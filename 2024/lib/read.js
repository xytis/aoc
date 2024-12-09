import fs from "fs";

import ArrayKeyedMap from "array-keyed-map"

export function read(name) {
    return  fs.readFileSync(name).toString()
}

export function readLines(name) {
    const fileContents = read(name)

    const lines = fileContents.split('\n')
    lines.pop()
    return lines;
}

export function readCoordMap(name) {
    const lines = readLines(name)
    const symbols = lines.map(l => l.split(''))
    const res = new ArrayKeyedMap()
    for (let y = 0, H = symbols.length; y < H; y++) {
        for (let x = 0, W = symbols[y].length; x < W; x++) {
            res.set([x, y], symbols[y][x])
        }
    }
    return res;
}

export function readCoordMapAndBounds(name) {
    const lines = readLines(name)
    const height = lines.length;
    const symbols = lines.map(l => l.split(''))
    const width = symbols[0].length;
    const res = new ArrayKeyedMap()
    for (let y = 0, H = symbols.length; y < H; y++) {
        for (let x = 0, W = symbols[y].length; x < W; x++) {
            res.set([x, y], symbols[y][x])
        }
    }
    return [res, [height, width]];
}

export function printCoordMap(m, conv) {
    let y = 0;
    let x = 0;
    let line = [];
    while (true) {
        let el = m.get([x, y])
        if (!el) {
            y = y + 1;
            x = 0;
            console.log(line.join(''))
            line = [];
            el = m.get([x, y])
        }
        if (!el) {
            break;
        }
        x = x + 1;
        line.push(conv(el));
    }
    console.log();
}
