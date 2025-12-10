
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

from heapq import heappush, heappop

def solve2(start, buttons):
    solved = lambda t: all(map(lambda e: e == 0, t))
    valid = lambda t: all(map(lambda e: e >= 0, t))
    score = lambda t: sum(t)
    queue = []
    heappush(queue, (score(start), start, 0))
    seen = {start}
    it = 0
    while len(queue) > 0:
        it = it + 1
        if it % 1000 == 0:
            print("space", len(seen))
            print("at", at)
        (s, at, cost) = heappop(queue)
        #print(s, at)
        # Prefer most impactful buttons first:
        buttons = sorted(buttons, key=lambda e: sum(at[i] for i in e), reverse=True)
        for v in reductions(at, buttons):
            if solved(v):
                return cost+1
            if valid(v) and v not in seen:
                seen.add(v)
                heappush(queue, (score(v), v, cost+1))


with open('thick.txt', 'r') as file:
    solutions1 = []
    solutions2 = []
    for line in file:
        t1, b, t2 = parse(line.strip())
        (cost1, path) = solve1(t1, b)
        cost2 = solve2(t2, b)
        print('SOLVED', t2, cost2)
        solutions1.append(cost1)
        solutions2.append(cost2)
    print(sum(solutions1))
    print(sum(solutions2))


