
sums = []
mult = []
results = []

with open('input.txt', 'r') as file:
    for line in file:
        column = 0
        nums = line.strip().split()
        for num in nums:
            # last line
            if num == '*':
                results.append(mult[column])
            elif num == '+':
                results.append(sums[column])
            else:
                num = int(num)
                if len(sums) == column:
                    sums.append(0)
                if len(mult) == column:
                    mult.append(1)
                sums[column] = sums[column] + num
                mult[column] = mult[column] * num
            column = column + 1

#print(sums)
#print(mult)
#print(results)
print(sum(results))

cols = []
ops = []
with open('input.txt', 'r') as file:
    for line in file:
        col = 0
        digs = list(line)
        for dig in digs:
            if dig == '*' or dig == '+':
                ops.append(dig)
            else:
                if len(cols) == col:
                    cols.append([])
                cols[col].append(dig)
            col = col + 1

import math

stack = []
#print(cols)
results = []

for col in cols:
    col = ''.join(col)
    col = col.strip()
    if col != '':
        stack.append(int(col))
    else:
        #print(stack)
        #print(ops[0])
        if ops[0] == '*':
            results.append(math.prod(stack))
        else:
            results.append(sum(stack))
        #print(results[-1])
        ops = ops[1:]
        stack = []

print(sum(results))
