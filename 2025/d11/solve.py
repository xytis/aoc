# Reverse propagate the number of connections + current value
from collections import defaultdict

def countr(t, f, rdag):
    rank = defaultdict(lambda: 0)
    # first count the realtive rank of each node
    # rank is necesary to understand in the second step, if a node has been fully
    # discovered, before propagating it's counts upwards.
    # (in the first attempt, I used number of children for this)
    queue = [f]
    visited = set()
    while len(queue) > 0:
        at = queue[0]
        queue = queue[1:]
        if at in visited:
            continue
        visited.add(at)
        for n in rdag[at]:
            rank[n] += 1
            queue.append(n)

    queue = [f]
    paths = defaultdict(lambda: 0)
    paths[f] = 1
    # propagate number of paths, starting from `f`
    visited = set()
    while len(queue) > 0:
        at = queue[0]
        queue = queue[1:]
        visited.add(at)
        for n in rdag[at]:
            rank[n] -= 1
            paths[n] = paths[n] + paths[at]
            if rank[n] == 0: # fully discovered, let's propagate.
                queue.append(n)

    return paths[t]

def countd(f, t, ddag):
    rank = defaultdict(lambda: 0)
    # first count the realtive rank of each node
    # rank is necesary to understand in the second step, if a node has been fully
    # discovered, before propagating it's counts upwards.
    # (in the first attempt, I used number of children for this)
    queue = [f]
    visited = set()
    while len(queue) > 0:
        at = queue[0]
        queue = queue[1:]
        if at in visited:
            continue
        visited.add(at)
        for n in ddag[at]:
            rank[n] += 1
            queue.append(n)

    queue = [f]
    paths = defaultdict(lambda: 0)
    paths[f] = 1
    # propagate number of paths, starting from `f`
    visited = set()
    while len(queue) > 0:
        at = queue[0]
        queue = queue[1:]
        visited.add(at)
        for n in ddag[at]:
            rank[n] -= 1
            paths[n] = paths[n] + paths[at]
            if rank[n] == 0: # fully discovered, let's propagate.
                queue.append(n)

    return paths[t]

with open('input.txt', 'r') as file:
    rdag = defaultdict(set)
    ddag = defaultdict(set)
    # build reverse dag and direct dag
    # I thought that there is a difference.
    for line in file:
        [n, r] = line.strip().split(':')
        for t in r.strip().split(' '):
            rdag[t].add(n)
            ddag[n].add(t)

    print('P1', countd('you', 'out', ddag))
    print('P1', countr('you', 'out', rdag))

    #print('svr -> fft', count('svr', 'fft', rdag))
    #print('fft -> dac', count('fft', 'dac', rdag))
    #print('dac -> out', count('dac', 'out', rdag))

    #print('svr -> dac', count('svr', 'dac', rdag))
    #print('dac -> fft', count('dac', 'fft', rdag))
    #print('fft -> out', count('fft', 'out', rdag))

    # takeway here: we either have fft -> dac or dac -> fft. Can't have cycles.
    # so one of these multiplications will be 0
    p2a = countr('svr', 'fft', rdag) * countr('fft', 'dac', rdag) * countr('dac', 'out', rdag)
    p2b = countr('svr', 'dac', rdag) * countr('dac', 'fft', rdag) * countr('fft', 'out', rdag)
    print('P2', p2a + p2b)

    p2a = countd('svr', 'fft', ddag) * countd('fft', 'dac', ddag) * countd('dac', 'out', ddag)
    p2b = countd('svr', 'dac', ddag) * countd('dac', 'fft', ddag) * countd('fft', 'out', ddag)
    print('P2', p2a + p2b)



