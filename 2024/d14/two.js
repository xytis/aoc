import {readLines} from "../lib/read.js";
import ArrayKeyedMap from "array-keyed-map";
import {lcm} from "../lib/math.js";

// const input = readLines("test.txt")
// const W = 11;
// const H = 7;
const input = readLines("input.txt")
const W = 101;
const H = 103;
const T = 0;

function clamp(c, S) {
    return ((c % S) + S) % S
}

function distance(px, py, tx, ty) {
    return Math.abs(tx - px) + Math.abs(ty - py);
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

function print(robots, w, h) {
    const positions = new ArrayKeyedMap();
    robots.forEach(([pos]) => {
        positions.set(pos, true);
    })
    for (let y = 0; y < h; y++) {
        let line = "";
        for (let x = 0; x < w; x++) {
            if (positions.has([x, y])) {
                line += "#"
            } else {
                line += "."
            }
        }
        console.log(line)
    }
    console.log();
}

const robots = input
    .map((line) => line.split(' '))
    .map(([up, uv]) => {
        const p = up.split('=')[1].split(',').map(i => parseInt(i, 10));
        const v = uv.split('=')[1].split(',').map(i => parseInt(i, 10));
        ;
        return [p, v]
    })
    .map(([[px, py], [vx, vy]]) => [[clamp(px + vx * T, W), clamp(py + vy * T, H)], [vx, vy]]);

let diffX = W;
let diffY = H;
let iter = 0;

let t = T;
while (t < T + 12000) {
    t++;

    let dispersion = 0;
    //step
    for (let robot of robots) {
        const [px, py] = robot[0];
        const [vx, vy] = robot[1];
        robot[0][0] = clamp(px + vx, W);
        robot[0][1] = clamp(py + vy, H);
        dispersion = dispersion + distance(robot[0][0], robot[0][1], 50, 51)
    }
    if (dispersion < 500 * 35
    ) {
        console.log("ITER", t, diffX, diffY);
        print(robots, W, H);
    }
}

// console.log(res)
