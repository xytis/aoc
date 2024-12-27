import {readCoordMap} from "../lib/read.js";
import ArrayKeyedMap from "array-keyed-map";
import {next} from "../lib/coords.js";

const world = readCoordMap("input-kk.txt");

function extractRegion(coords, plant, region) {
    if (world.get(coords) === plant) {
        world.delete(coords);
        region.set(coords, plant);
        [0, 2, 4, 6].forEach(direction => {
            extractRegion(next(coords, direction), plant, region);
        })
    }
}

function sides(region) {
    // const plant = [...region.values()][0];
    // const one = [...region.keys()][0];
    const walls = new ArrayKeyedMap();
    [...region.keys()].forEach((coords) => {
        [
            [0, [0,0], [1,0]],
            [2, [1,0], [1,1]],
            [4, [1,1], [0,1]],
            [6, [0,1], [0,0]],
        ].forEach(([dir, [xa, ya], [xb, yb]]) => {
            if (region.has(next(coords, dir))) {
                // Not a boundary
                return;
            }
            const [x, y] = coords;
            const a = [x+xa, y+ya, dir];
            const b = [x+xb, y+yb, dir];
            if (walls.has(a)) {
                walls.delete(a);
            } else {
                walls.set(a, true);
            }
            if (walls.has(b)) {
                walls.delete(b);
            } else {
                walls.set(b, true);
            }
        }, 0);
        // console.log(...walls.keys());
    })
    // console.log(one, plant, walls.size/2, region.size);
    return walls.size/2;
}

const regions = [...world.entries()].reduce((regions, [coords, plant]) => {
    if (world.has(coords)) {
        const region = new ArrayKeyedMap();
        extractRegion(coords, plant, region);
        regions.push(region);
        return regions;
    }
    return regions;
}, [])

const result = regions.map(region => {
    const area = region.size;
    // return [region.size, perimeter(region)];
    return area * sides(region);
}).reduce((a, b) => a + b);

console.log(result);
