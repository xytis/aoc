import {readLines} from "../lib/read.js";
import {memo, pairs} from "../lib/utils.js";
import {Graph} from "../lib/graph.js";

const graph = new Graph();

readLines("input.txt").forEach((edge) => {
    const [s, d] = edge.trim().split('-');
    graph.set(s, d, 0);
    graph.set(d, s, 0);
})

function meshify(neighbors) {
    return [...neighbors].sort().join(',')
}

const meshes = [];

// console.log(graph)

const largestMesh = memo((key, neighbors) =>{
    const mesh = pairs([...neighbors]).every(([a, b]) => graph.has(a, b))
    if (mesh) {
        return neighbors;
    }
    let largest = new Set();
    for (const neighbor of neighbors) {
        const without = new Set([...neighbors]);
        without.delete(neighbor);
        const attempt = largestMesh([...without].join(':'), without);
        if (attempt.size > largest.size) {
            largest = attempt;
        }
    }
    return largest;
})

// Check each node for the largest mesh they can form
for (const source of graph.sources()) {
    const nodes = new Set([source, ...graph.destinations(source)]);
    // We need to evaluate recursively if the node set is a full mesh.
    // If not, we branch by removing one node and checking again.
    const mesh = largestMesh([...nodes].join(':'), nodes);
    // Save the found mesh
    meshes.push(meshify(mesh));
    // Remove the node from further investigation.
    graph.delete(source);
}

console.log(meshes);

const max = meshes.reduce((acc, cur) => {
    if (acc.length < cur.length) {
        acc = cur;
    }
    return acc;
}, "")

console.log(max)
