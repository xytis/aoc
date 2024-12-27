export function range(size, startAt = 0) {
    return [...Array(size).keys()].map(i => i + startAt);
}

export function *take(array, chunk = 1, every =1) {
    for (let i = 0; i < array.length; i += every) {
        yield array.slice(i, i + chunk);
    }
}

export function memo(target) {
    const cache = new Map();
    const f = (...args) => {
        const key = args.join(",");
        if (cache.has(key)) {
            return cache.get(key);
        }
        const val = target(...args);
        cache.set(key, val);
        return val;
    }
    f.clear = () => {
        cache.clear();
    }
    return f;
}

export const pairs = (arr) => arr.map((v, i) => arr.slice(i + 1).map(w => [v, w])).flat();
