import math
from itertools import combinations

corners = [tuple(map(int, line.split(','))) for line in open('input.txt')]


def area(a, b):
    (x1, y1) = a
    (x2, y2) = b
    return (abs(x1-x2)+1)*(abs(y1-y2)+1)

areas = map(lambda p: area(*p), combinations(corners, 2))

# part 1
print(max(areas))

from sympy.geometry import Point, Polygon, Segment, intersection

# we need to revert Y axis, because the initial coords have been in screen system, and the rest of math works in paper :)
#corners = list(map(lambda p: (p[0], -p[1]), corners))

def expand(a, b):
    (x1, y1) = a
    (x2, y2) = b
    # We need to ensure the rectangle is in clockwise direction
    # There are only two orders (because of axis alignment), so we don't need fancy sort.
    if x1 <= x2:
        if y1 <= y2:
            return Polygon(a, (x2, y1), b, (x1, y2))
        else:
            return Polygon(a, (x1, y2), b, (x2, y1))
    else:
        if y1 <= y2:
            return Polygon(a, (x1, y2), b, (x2, y1))
        else:
            return Polygon(a, (x2, y1), b, (x1, y2))

def is_on_border(point, polygon):
    for segment in polygon.sides:
        if point in segment:
            return True
    return False

def inside(a, b):
    if isinstance(b, Polygon):
        #print("poly", b)
        # check for line intesections
        for side in b.sides:
            #print("side", side)
            # if any of the intersections are points, we have crossings or touching.
            # all other segments are fine, because the next check will see if the segments end in correct locations.
            for x in a.intersection(side):
                if not isinstance(x, Segment):
                    if not x in side.points:
                        #print("nope: point", x)
                        return False
                    #print("skip: endpoint", x)
        # check if all vertices are inside
        for corner in b.vertices:
            if not a.encloses_point(corner) and not is_on_border(corner, a):
                #print("nope: outside", corner)
                return False
        return True
    # We don't care for 1 width polygons, because they will not be in the answer anyway.
    return False

def intarea(p):
    [a, _, b, _] = p.vertices
    return area(a, b)

tiled = Polygon(*corners)

allrects = list(map(lambda p: expand(*p), combinations(corners, 2)))

#print("INNERS", allrects)
#for i, r in enumerate(allrects):
#    print(i, r)

print(max(map(intarea, filter(lambda r: inside(tiled, r), allrects))))
