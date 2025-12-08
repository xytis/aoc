import math
from itertools import combinations

positions = [tuple(map(int, line.split(','))) for line in open('input.txt')]

# *(,) does explode.
pairs = sorted(combinations(positions, 2), key=lambda p: math.dist(*p))

# we need frozenset because we want to have a set of them. But also we want to perform union on them.
groups = {frozenset([p]) for p in positions}

for i, (a,b) in enumerate(pairs):
    if i == 1000:
        print('part 1', math.prod(map(len, sorted(groups, key=len)[-3:])))

    # find the clusters to which the points belong
    A = next(c for c in groups if a in c)
    B = next(c for c in groups if b in c)
    # remove the clusters we are about to merge
    groups -= {A, B}

    # if we removed last two clusters, we are about to complete the graph
    if not groups:
        print('part 2', a[0]*b[0])
        break

    # return the merged cluster
    groups.add(A | B)

