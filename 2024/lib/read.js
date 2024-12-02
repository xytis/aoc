import fs from "fs";

export function readLines(name) {
    const fileContents = fs.readFileSync(name).toString()

    const lines = fileContents.split('\n')
    lines.pop()
    return lines;
}
