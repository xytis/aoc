// Graph is a key-value of this form:
// Map source: Map destinations: costs
export class Graph extends Map {
    constructor(edges) {
        super();
        // Edge [from, to, cost]
        if (edges) {
            this.parse(edges);
        }
    }

    parse(edges) {
        for (const edge of edges) {
            const [s, d, c] = edge;
            this.set(s, d, c);
        }
    }

    has(source, destination) {
        const connections = super.get(source) || new Map();
        return connections.has(destination);
    }

    set(source, destination, cost) {
        const connections = super.get(source) || new Map();
        connections.set(destination, cost);
        return super.set(source, connections);
    }

    get(source, destination) {
        const connections = super.get(source) || new Map();
        return connections.get(destination);
    }

    delete(source, destination) {
        if (destination === undefined) {
            // We are deleting a node, not an edge,
            // in that case, purge all instances of this node
            for (const [s, d, _] of this.entries()) {
                if (s === source) this.delete(s, d);
                if (d === source) this.delete(s, d);
            }
        }
        const connections = super.get(source) || new Map();
        connections.delete(destination);
        if (connections.size === 0) {
            super.delete(source);
        }
    }

    *entries() {
        for (const [source, connections] of super.entries()) {
            for (const [destination, cost] of connections.entries()) {
                yield [source, destination, cost];
            }
        }
    }

    *sources() {
        yield *super.keys();
    }

    *destinations(source) {
        if (source) {
            yield *super.get(source).keys();
            return;
        }
        const shown = new Set();
        for (const connections of super.values()) {
            for (const destination of connections.keys()) {
                if (!shown.has(destination)) {
                    shown.add(destination);
                    yield destination;
                }
            }
        }
    }
}
