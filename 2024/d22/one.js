/*
Calculate the result of multiplying the secret number by 64. Then, mix this result into the secret number. Finally, prune the secret number.
Calculate the result of dividing the secret number by 32. Round the result down to the nearest integer. Then, mix this result into the secret number. Finally, prune the secret number.
Calculate the result of multiplying the secret number by 2048. Then, mix this result into the secret number. Finally, prune the secret number.
Each step of the above process involves mixing and pruning:

To mix a value into the secret number, calculate the bitwise XOR of the given value and the secret number. Then, the secret number becomes the result of that operation. (If the secret number is 42 and you were to mix 15 into the secret number, the secret number would become 37.)
To prune the secret number, calculate the value of the secret number modulo 16777216. Then, the secret number becomes the result of that operation. (If the secret number is 100000000 and you were to prune the secret number, the secret number would become 16113920.)

 */

import {range} from "../lib/utils.js";
import {readLines} from "../lib/read.js";

function mix(num, value) {
    return value ^ num;
}

function prune(num) {
    return num % 16777216n;
}

function div(num, value) {
    return num/value;
}

function mul(num, value) {
    return num * value;
}

function generate(num) {
    num = prune(mix(num, mul(num, 64n)));
    num = prune(mix(num, div(num, 32n)));
    num = prune(mix(num, mul(num, 2048n)));
    return num;
}

//console.log(range(10).reduce((prev) => generate(prev), 123n))

/*
15887950
16495136
527345
704524
1553684
12683156
11100544
12249484
7753432
5908254
 */

function n2000(start) {
    return range(2000).reduce((prev) => generate(prev), start)
}

const input = readLines("input.txt").map(x => BigInt(x.trim()));

const result = input.reduce((sum, n) => sum = sum + n2000(n), 0n);

console.log(result);
