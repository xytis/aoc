def funny(num):
    txt = str(num)
    middle = len(txt)//2
    halfA = txt[0:middle]
    halfB = txt[middle:]
    return halfA == halfB

def silly(num):
    txt = str(num)
    if re.search(r"^(\d{1,})\1{1,}$", txt):
        return True
    return False

import re

funnies = []

with open('input.txt','r') as file:
    for line in file:
        for rang in line.split(','):
            [start, end] = rang.split('-');
            start = int(start)
            end = int(end)
            for num in range(start, end+1):
                if funny(num):
                    funnies.append(num)

#print(funnies)
print(sum(funnies))

sillies = []

with open('input.txt','r') as file:
    for line in file:
        for rang in line.split(','):
            [start, end] = rang.split('-');
            start = int(start)
            end = int(end)
            for num in range(start, end+1):
                if silly(num):
                    sillies.append(num)

#print(sillies)
print(sum(sillies))
