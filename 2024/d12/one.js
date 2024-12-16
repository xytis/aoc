import {readCoordMap} from "../lib/read.js";
import ArrayKeyedMap from "array-keyed-map";
import {next} from "../lib/coords.js";

const world = readCoordMap("input.txt");

function extractRegion(coords, plant, region) {
    if (world.get(coords) === plant) {
        world.delete(coords);
        region.set(coords, plant);
        [0, 2, 4, 6].forEach(direction => {
            extractRegion(next(coords, direction), plant, region);
        })
    }
}

function perimeter(region) {
    return [...region.keys()].reduce((perimeter, coords) => {
        const walls = [0, 2, 4, 6].reduce((walls, dir) => {
            if (region.has(next(coords, dir))) {
                return walls;
            }
            return walls + 1;
        }, 0);
        return perimeter + walls;
    }, 0)
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
    // return [region.size, perimeter(region)];
    return region.size * perimeter(region);
}).reduce((a, b) => a + b);

console.log(result);
