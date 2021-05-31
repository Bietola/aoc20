import fileinput
from itertools import combinations

inp = map(
    int,
    fileinput.input()
)

sols = next(filter(lambda x: x[0] + x[1] + x[2] == 2020, combinations(inp, 3)))

print(sols[0] * sols[1] * sols[2])
