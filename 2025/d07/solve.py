def visual(g):
    y = 0
    while True:
        x = 0
        if (x, y) not in g:
            break
        while True:
            if (x, y) not in g:
                break
            print(g[(x, y)], end='')
            x = x + 1
        print()
        y = y + 1
    print()


def grid(f):
    g = {}
    with open(f, 'r') as file:
        y = 0
        for line in file:
            x = 0
            for sym in list(line.strip()):
                g[(x, y)] = sym
                x = x + 1
            y = y + 1
    return g

def bounds(g):
    minx, miny, maxx, maxy = 0,0,0,0
    for (x, y) in g.keys():
        if x < minx:
            minx = x
        if x > maxx:
            maxx = x
        if y < miny:
            miny = y
        if y > maxy:
            maxy = y
    return (minx, miny), (maxx, maxy)

def square(s, e):
    return [(x, y) for x in range(s, e+1) for y in range(s, e+1)]

def neighbours(c, g):
    (cx, cy) = c
    n = []
    for (dx, dy) in square(-1, 1):
        dc = (cx + dx, cy + dy)
        if dc != c and dc in g:
            n.append(g[dc])
    return n

def filter(s, a):
    return [x for x in a if x == s]

def count(s, a):
    return len(filter(s, a))

def split(s):
    (x, y) = s
    return (x-1, y),(x+1, y)

def down(s):
    (x,y) = s
    return (x, y+1)

g = grid('input.txt')
#visual(g)

# Find first tachyon
s = list(g.keys())[list(g.values()).index('S')]
tachyons = {s}
splits = 0

#print(tachyons)
while len(tachyons) > 0:
    iteration = set()
    for tach in tachyons:
        at = down(tach)
        if at in g:
            el = g[at]
            if el == '^':
                splits = splits+1
                l, r = split(at)
                iteration.add(l)
                iteration.add(r)
            else:
                iteration.add(at)
    #print(iteration)
    #print(splits)
    tachyons = iteration

print(splits)

from PIL import Image, ImageDraw
color_empty = (0,0,0)
color_split = (128,128,0)
#color_tachi = (200,200,200)

minc, maxc = bounds(g)
(width, heigth) = maxc
width = width + 1
heigth = (heigth+1)

frames = []
def frame(g, t):
    im = Image.new('RGB', (width, heigth), color_empty)
    for c in g.keys():
        (x, y) = c
        if g[c] == '^':
            im.putpixel((x, y), color_split)
        else:
            if c in t:
                r_ = (t[c]) % 90
                g_ = (t[c]+30) % 90
                b_ = (t[c]+60) % 90
                im.putpixel((x, y), (140+r_, 140+g_, 140+b_))
    frames.append(im)

# Find first tachyon
s = list(g.keys())[list(g.values()).index('S')]
timelines = {s:1}
tails = timelines
total = 0
frame(g, tails)
while len(timelines) > 0:
    iteration = {}
    for at in timelines.keys():
        num = timelines[at]
        at = down(at)
        if at in g:
            el = g[at]
            if el == '^':
                l, r = split(at)
                if l in iteration:
                    iteration[l] = iteration[l] + num
                else:
                    iteration[l] = num
                if r in iteration:
                    iteration[r] = iteration[r] + num
                else:
                    iteration[r] = num
            else:
                if at in iteration:
                    iteration[at] = iteration[at] + num
                else:
                    iteration[at] = num
        else:
            total = total + num
    #print(iteration)
    tails = tails | iteration
    timelines = iteration
    frame(g, tails)

print(total)

frames[0].save('anim.gif',
               save_all = True, append_images = frames[1:],
               optimize = False, duration = 10)
