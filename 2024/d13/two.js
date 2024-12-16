import {read} from "../lib/read.js";
import {solve} from "../lib/linear.js";

const EPS = 0.01

const cases = read("input.txt").split('\n\n')
    .map((c) => {
        const [A, B, T] = c.split('\n');
        const [Ax, Ay] = A.split(':')[1].trim().split(',').map(c => c.trim().split('+')[1]);
        const [Bx, By] = B.split(':')[1].trim().split(',').map(c => c.trim().split('+')[1]);
        const [Tx, Ty] = T.split(':')[1].trim().split(',').map(c => c.trim().split('=')[1]);

        return [Ax, Ay, Bx, By, Tx, Ty].map(a => parseInt(a, 10));
    })
    .map(([Ax, Ay, Bx, By, Tx, Ty]) => {
        return solve([[Ax, Bx], [Ay, By]], [Tx+10000000000000, Ty+10000000000000]);
    })
    .filter(([u, v]) => {
        return (u > 0 && v > 0)
            && (Math.abs(u - Math.round(u)) < EPS) && (Math.abs(v - Math.round(v)) < EPS)
    })
    .map(([u, v]) => Math.round(u) * 3 + Math.round(v)).reduce((a, b) => a + b);

// Equasions:
// U = Ax + Ay
// V = Bx + By
// T = u U + v V
// then
// Tx = u Ax + v Bx
// Ty = u Ay + v By
// or:
// |Ax Bx| |u|  |Tx|
// |Ay By| |v|  |Ty|

console.log(cases);
