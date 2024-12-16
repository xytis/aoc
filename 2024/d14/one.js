import {readLines} from "../lib/read.js";

const T = 100;
// const W = 11;
// const H = 7;
const W = 101;
const H = 103;

function clamp(c, S) {
    return ((c % S) + S) % S
}

function quadrant(px, py, w, h) {
    // 0 1
    // 2 3
    // rest - 5
    const mw = (w - 1) / 2;
    const mh = (h - 1) / 2;
    if (px < mw) {
        if (py < mh) return 0;
        if (py > mh) return 2;
        return 5;
    }
    if (px > mw) {
        if (py < mh) return 1;
        if (py > mh) return 3;
        return 5;
    }
    return 5;
}

const robots = readLines("input.txt")
    .map((line) => line.split(' '))
    .map(([up, uv]) => {
        const p = up.split('=')[1].split(',').map(i => parseInt(i, 10));
        const v = uv.split('=')[1].split(',').map(i => parseInt(i, 10));;
        return [p, v]
    })
    .map(([[px, py], [vx, vy]]) => [px + vx * T, py + vy * T])
    .map(([px, py]) => [clamp(px, W), clamp(py, H)])
    .map(([px, py]) => [px, py, quadrant(px, py,W, H)])
    .reduce((acc, [_x, _y, q]) => {
        acc[q] = (acc[q] || 0) + 1;
        return acc;
    }, {})


console.log(robots[0] * robots[1] * robots[2] * robots[3]);
