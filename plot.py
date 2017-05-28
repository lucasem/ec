
# usage: python plot.py out.tsv
# produces dem.eps, showing learning curve (% tasks solved vs iteration) for various frontier sizes.

import sys
import os
from math import exp
from collections import defaultdict
from functools import reduce
from itertools import combinations

from matplotlib import pyplot as plt

if len(sys.argv) < 2:
    print("must TSV file")
    exit(1)


def make_table(filename): # {(smoothing, lambda): {frontier_size: [hit_rate]}}}
    tab = defaultdict(lambda: defaultdict(list))
    with open(filename) as f:
        for line in f:
            datum = line.strip().split("\t")
            frontier_size, it, l, s, hit_rate = datum[:5]
            frontier_size, it, l, s, hit_rate = int(frontier_size), int(it), float(l), float(s), int(hit_rate)/45
            l = (l // 0.1) / 10 # because of some weird ec behavior with l=1.0
            tab[(s,l)][frontier_size].append(hit_rate)
    return tab

table = make_table(sys.argv[1])

smoothings = set(map(lambda k:k[0], table.keys()))
lambdas = set(map(lambda k:k[1], table.keys()))
x, y = len(smoothings), len(lambdas)

# After running the code further below (after this block),
# I decided on the best parameters for demonstration.
for frontier_size in table[(1.0, 1.0)]:
    vals = table[(1.0, 1.0)][frontier_size]
    plt.plot(list(range(1,1+len(vals))), vals, label=f"{frontier_size}")
plt.savefig('res.eps')
plt.legend()
plt.ylabel('% tasks solved')
plt.xlabel('iteration')
plt.savefig('dem.eps')
exit()

plt.figure(figsize=(12,12))
n = 1
for k in table:
    plt.subplot(x, y, n)
    for frontier_size in table[k]:
        plt.plot(table[k][frontier_size], label=f"{frontier_size}")
    plt.title(f"smoothing {k[0]} ; lambda {k[1]}")
    plt.legend()
    plt.ylabel('% tasks solved')
    plt.xlabel('iteration')
    n += 1
plt.tight_layout()
plt.savefig('res.eps')
