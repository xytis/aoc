import {readLines} from "../lib/read.js";
import {pairs} from "../lib/utils.js";
import {Graph} from "../lib/graph.js";

const graph = new Graph();

readLines("input.txt").forEach((edge) => {
    const [s, d] = edge.trim().split('-');
    graph.set(s, d, 0);
    graph.set(d, s, 0);
})

function triplet(a, b, c) {
    return [a, b, c].sort().join('-')
}

const triplets = new Set();

// We need to consume the graph in order to get triplets.
for (const source of graph.sources().filter(source => source[0] === 't')) {
    const neighbours = [...graph.destinations(source)];
    // All connections are triplets.
    if (neighbours.length >= 2) {
        for (const [a, b] of pairs(neighbours)) {
            if (graph.has(a, b)) triplets.add(triplet(a, b, source));
        }
    }
    // Remove the node from further investigation.
    graph.delete(source);
}

console.log(triplets.size);

