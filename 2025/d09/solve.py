import math
from itertools import combinations, pairwise

corners = [tuple(map(int, line.split(','))) for line in open('input.txt')]


def area(a, b):
    (x1, y1) = a
    (x2, y2) = b
    return (abs(x1-x2)+1)*(abs(y1-y2)+1)

areas = map(lambda p: area(*p), combinations(corners, 2))

# part 1
print(max(areas))

# part 2
# construct all posible rectangles (and save in aabb form)
# check if any of the rectangle lines (which are always axis aligned) cross any of the polygon lines.
# that can be done if we imagine both rectangle and the polygon line as a aabb.
# we tolerate touch, because if we have some funky indents, another polygon line will strike out the
# rectangle.

def expand(a, b):
    (x1, y1) = a
    (x2, y2) = b
    # build a aabb from the rectangle
    return ( (min(x1, x2), min(y1, y2)), (max(x1, x2), max(y1, y2)) )

def crosses(aabb, line):
    ((xmin, ymin), (xmax, ymax)) = aabb
    ((x1, y1), (x2, y2)) = line
    # convert line to "aabb"
    umin = min(x1, x2)
    umax = max(x1, x2)
    vmin = min(y1, y2)
    vmax = max(y1, y2)

    # touch toleration as equality
    arightb = umin >= xmax
    aleftb = umax <= xmin
    adownb = vmin >= ymax
    aupb = vmax <= ymin

    return not (arightb or aleftb or adownb or aupb)


rectangles = list(map(lambda p: expand(*p), combinations(corners, 2)))

# close the polygon
corners.append(corners[0])

polylines = list(pairwise(corners))

m = 0
for r in rectangles:
    ok = True
    for l in polylines:
        if crosses(r, l):
            ok = False
            break;
    if ok:
        #print("good", r, l)
        m = max(area(*r), m)

print(m)
