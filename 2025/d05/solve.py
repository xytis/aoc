from intervaltree import Interval, IntervalTree

tree = IntervalTree()

mode = True

fresh = []

with open('input.txt', 'r') as file:
    for line in file:
        line = line.strip()
        if line == '':
            mode = False
            continue
        if mode:
            [b,e] = line.split('-')
            b = int(b)
            e = int(e) + 1
            tree[b:e] = None
        else:
            e = int(line)
            if len(tree.overlap(e, e+1)) > 0:
                fresh.append(e)


print(len(fresh))
#print(tree)

tree.merge_overlaps(strict=False)
total = 0
for i in sorted(tree):
    total = total + i.length()

print(total)



