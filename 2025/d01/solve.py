count = 50
zeros = 0

# ONE
with open('input.txt', 'r') as file:
    for line in file:
        line = line.strip()
        dir = line[0]
        num = int(line[1:])
        match dir:
            case 'L':
                count = (count - num) % 100
            case 'R':
                count = (count + num) % 100
        if count == 0:
            zeros = zeros + 1

print(zeros)


#TWO
index = 50
zeros = 0

with open('input.txt', 'r') as file:
    for line in file:
        line = line.strip()
        #print(line, '->z', zeros, '->i', index, end=' ')
        dir = line[0]
        offset = int(line[1:])

        start = index
        end = index
        spins = offset // 100
        offset = offset % 100
        cross = False
        match dir:
            case 'L':
                end = (index - offset) % 100
                cross = (index - offset) < 0
            case 'R':
                end = (index + offset) % 100
                cross = (index + offset) > 99
        index = end
        if start == 0:
            zeros = zeros + spins
            #print('z->', zeros)
            continue
        if end == 0:
            zeros = zeros + spins + 1
            #print('z->', zeros)
            continue
        if cross:
            zeros = zeros + spins + 1
            #print('z->', zeros)
            continue
        zeros = zeros + spins
        #print('z->', zeros)

print(zeros)
