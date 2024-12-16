export function gcd(a, b) {
    if (!b) {
        return a;
    }

    return gcd(b, a % b);
}
export function lcm(a, b) {
    return Math.abs(a*b)/gcd(a, b);
}
