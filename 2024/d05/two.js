import {readLines} from "../lib/read.js";

// const lines = readLines('./test1.txt');
const lines = readLines('./input.txt');

const [rules, printouts] = lines.reduce(([rules, printouts, s], line) => {
    if (line === "") {
        return [rules, printouts, true];
    }
    if (s) {
        printouts.push(line)
    } else {
        rules.add(line)
    }
    return [rules, printouts, s]
}, [new Set(), [], false])

function middle(list) {
    return list[Math.floor((list.length - 1) / 2)];
}

const cases = printouts.map((line) => {
    const pages = line.split(',').map(p => parseInt(p, 10));
    const inverseFacts = new Set();
    for (let i = 0; i < pages.length-1; i++) {
        for (let j = i + 1; j < pages.length; j++) {
            inverseFacts.add(`${pages[j]}|${pages[i]}`);
        }
    }
    return [pages, inverseFacts, middle(pages)];
})

const incorrect = cases.filter(([_, inverseFacts, middle]) => {
    return inverseFacts.intersection(rules).size > 0
});

const after = rules.values().reduce((acc, rule) => {
    const [b, a] = rule.split('|').map(n => parseInt(n, 10));
    acc.set(a, acc.get(a) || new Set());
    acc.get(a).add(b);
    return acc;
}, new Map());

const sorted = incorrect.map(([pages, _1, _2]) => {
    let subset = new Set(pages);

    const filteredRules = (rules, subset) => {
        const result = new Map()
        for (let [page, origins] of rules) {
            if (subset.has(page)) {
                result.set(page, origins.intersection(subset));
            }
        }
        return result;
    }

    let aplicable = filteredRules(after, subset);

    for (let [page, origins] of after) {
        if (subset.has(page)) {
            aplicable.set(page, origins.intersection(subset));
        }
    }
    // We now need to find the orphan page, and push it into the output.
    const ordered = [];
    const orphan = function (subset, applicable) {
        return subset.values().find((p) => {
            if (!aplicable.has(p)) {
                return true;
            }
            const parents = applicable.get(p);
            return parents.size === 0;
        })
    }

    while (subset.size > 0) {
        const o = orphan(subset, aplicable);
        ordered.push(o);
        subset.delete(o);
        aplicable = filteredRules(aplicable, subset);
    }

    return [ordered, middle(ordered)]
})

const result = sorted.reduce((s, [_, e]) => s+e, 0);

console.log(result);

