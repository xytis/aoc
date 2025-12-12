
with open('input.txt', 'r') as file:
    # each input is of size 7 (so I don't even parse)
    fits = 0
    for line in file:
        prob = line.split(':')
        if len(prob) < 2 or prob[1] == '\n':
            continue
        # let's check if given dimensions are enough for the boxes
        x, y = map(int, prob[0].split("x"))
        required = sum(map(lambda v: int(v) * 7, prob[1].strip().split(" ")))
        if x * y > required:
            fits += 1

    print(fits)
