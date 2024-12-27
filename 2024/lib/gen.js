
const a = function* (level){
    console.log("a:", level);
    if (level < 0) {
        yield "*";
        return;
    }
    yield `(${level}:`;
    for (let i = 0; i < level; i++) {
        yield* a(level - 1);
    }
    yield `:${level})`;
}

const b = function(level) {
    console.log("b:", level);
    if (level < 0) {
        return "*";
    }
    const inner = [];
    for (let i = 0; i < level; i++) {
        inner.push(...b(level - 1));
    }
    return [`(${level}:`, ...inner, `:${level})`]
}

console.log([...(a(3).take(9))].join(""));
console.log([...b(3)].join(""));
