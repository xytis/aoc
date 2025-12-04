jolts = []

with open('input.txt', 'r') as file:
    for line in file:
        line = list(line.strip())
        m = max(line[:-1])
        i = line.index(m)
        a = line[i]
        b = max(line[i+1:])
        num = int(a + b)
        jolts.append(num)

print(sum(jolts))

joltages = []

with open('input.txt', 'r') as file:
    for line in file:
        line = list(line.strip())
        num = []
        # first 11 digits
        for rem in range(-11, 0, 1):
            m = max(line[:rem])
            i = line.index(m)
            num.append(m)
            line = line[i+1:]
        # final digit
        m = max(line)
        num.append(m)

        num = int(''.join(num))
        joltages.append(num)

#print(joltages)
print(sum(joltages))
