// directions:
//
// 7 0 1
// 6 X 2
// 5 4 3
//
export function next([x, y], dir) {
    switch (dir) {
        case 'X':
            return [x, y];
        case 0:
            return [x, y - 1];
        case 1:
            return [x + 1, y - 1];
        case 2:
            return [x + 1, y];
        case 3:
            return [x + 1, y + 1];
        case 4:
            return [x, y + 1];
        case 5:
            return [x - 1, y + 1];
        case 6:
            return [x - 1, y];
        case 7:
            return [x - 1, y - 1];
    }
}

export function check(coords, dir, pattern, map) {
    if (pattern.length === 0) {
        // Full pattern matched
        return true;
    }
    if (!coords) {
        // Out of bounds for next step
        return false;
    }
    const [cur, ...rest] = pattern;
    const on = map.get(coords);
    if (cur !== on) {
        // Pattern mismatch
        return false;
    }
    return check(next(coords, dir), dir, rest, map);
}

export function turnRight(dir) {
    return (dir + 2) % 8;
}
