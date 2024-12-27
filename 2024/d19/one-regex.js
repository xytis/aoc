const input = `r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
`

const [patternsInput, targetsInput] = input.split("\n\n")

const regex = new RegExp(`^(${patternsInput.trim().split(", ").join('|')})*$`)

const targets = targetsInput.trim().split("\n");

console.log(targets.map(regex.test).filter(Boolean).length);
