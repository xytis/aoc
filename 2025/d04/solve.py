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






g = grid('input.txt')
#visual(g)
n = {at: count('@', neighbours(at, g)) for at in g.keys()}
#visual(n)
r = {at: int(n[at] < 4 and g[at] == '@') for at in g.keys()}
#visual(r)

print(sum(r.values()))


g = grid('input.txt')

minc, maxc = bounds(g)
print(minc, maxc)
removed = []

from PIL import Image, ImageDraw
color_dot = (0,255, 0)
color_rem = (0,0,255)
color_full = (255, 0, 0)

frames = []
def frame(g, r):
    (x, y) = maxc
    im = Image.new('RGB', (x+1, y+1), color_dot)
    for c in g.keys():
        if r[c] == 0:
            if g[c] == '@':
                im.putpixel(c, color_full)
            else:
                im.putpixel(c, color_dot)
        else:
            im.putpixel(c, color_rem)
    frames.append(im)



while True:
    n = {at: count('@', neighbours(at, g)) for at in g.keys()}
    #visual(n)
    r = {at: int(n[at] < 4 and g[at] == '@') for at in g.keys()}
    #visual(r)
    rem = sum(r.values())
    if rem == 0:
        break
    removed.append(rem)
    #visual(g)
    g = {at: '.' if r[at] == 1 else g[at] for at in g.keys()}
    #visual(g)
    frame(g, r)

print(sum(removed))

frames[0].save('anim.gif',
               save_all = True, append_images = frames[1:],
               optimize = False, duration = 10)
