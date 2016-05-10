import sys

with open(sys.argv[1], encoding='utf8') as inputfile:
    for line in inputfile:
        line = line.strip('\n')
        pos = line.find(';')
        if pos == -1:
            print(line)
        else:
            print(line[:pos])
