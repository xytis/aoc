
def parse(line):
    target = ""
    buttons = []
    joltage = ()
    for snip in line.split(' '):
        if snip[0] == '[':
            snip = snip[1:-1]
            target = snip
        elif snip[0] == '(':
            snip = snip[1:-1]
            ids = list(map(int, snip.split(',')))
            buttons.append(ids)
        elif snip[0] == '{':
            snip = snip[1:-1]
            ids = list(map(int, snip.split(',')))
            joltage = tuple(ids)
    return target, buttons, joltage

def toggle(at, button):
    res = list(at)
    for i in button:
        if at[i] == '.':
            res[i] = '#'
        else:
            res[i] = '.'
    res = ''.join(res)
    #print(at, button, res)
    return res

def variants(at, buttons):
    #print('expand')
    res = []
    for b in buttons:
        res.append((toggle(at, b), b))
    return res


def solve1(target, buttons):
    solution = '.'*len(target)
    queue = [(target, 0, [])]
    seen = {target}
    while len(queue) > 0:
        (at, cost, path) = queue[0]
        queue = queue[1:]
        for (v, b) in variants(at, buttons):
            if v == solution:
                return (cost+1, path + [b])
            if v not in seen:
                seen.add(v)
                queue.append((v, cost+1, path + [b]))

def reduce(at, button):
    at = list(at)
    for i in button:
        at[i] = at[i]-1
    return tuple(at)

def reductions(at, buttons):
    res = []
    for b in buttons:
        res.append(reduce(at, b))
    return res

import numpy as np
from scipy.optimize import linprog

def solve2(target, buttons):
    # convert buttons into vectors
    vecs = []
    for b in buttons:
        l = [0] * len(target)
        for i in b:
            l[i] = 1
        vecs.append(np.array(l))
    c = [1] * len(buttons)
    A_eq = np.vstack(vecs).T
    target = np.array(target)

    result = linprog(
        c,
        A_eq=A_eq,
        b_eq=target,
        bounds=(0, None),
        integrality=1,
        #method='highs'
    )
    return int(result.fun)


with open('input.txt', 'r') as file:
    solutions1 = []
    solutions2 = []
    for line in file:
        t1, b, t2 = parse(line.strip())
        (cost1, _) = solve1(t1, b)
        cost2 = solve2(t2, b)
        #print('SOLVED', t2, cost2)
        solutions1.append(cost1)
        solutions2.append(cost2)
    print(sum(solutions1))
    print(sum(solutions2))


