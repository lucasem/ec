# usage: python plot.py out.tsv
# produces results.eps, showing learning curve (% tasks solved vs iteration) for various frontier sizes.

import sys
import os
from math import exp
from collections import defaultdict
from functools import reduce
from itertools import combinations

from matplotlib import pyplot as plt

OUTPUT = 'learning_curves.eps'

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

# prevent lines from overlapping each other in the plot
# by marginally increasing the value of one of them.
# returns something that might exactly be table[key][frontier_size]
def non_overlapping(table, k, frontier_size, error=0.005, exclude=None):
    vals = table[(1.0, 1.0)][frontier_size].copy()
    for i, val in enumerate(vals):
        if exclude and i in exclude: continue
        # largest value no greater than the current one, only considering smaller frontiers
        other = max(
                filter(lambda v: v <= val,
                    map(lambda f: table[k][f][i],
                        filter(lambda f: f<frontier_size,
                            table[k].keys()))),
                default=-1)
        if abs(other-val) < error:
            vals[i] = val + error
    return vals

table = make_table(sys.argv[1])

smoothings = set(map(lambda k:k[0], table.keys()))
lambdas = set(map(lambda k:k[1], table.keys()))
x, y = len(smoothings), len(lambdas)

# After running the code further below (after this block),
# I decided on the best parameters for demonstration.
#for frontier_size in table[(1.0, 1.0)]:
#    vals = table[(1.0, 1.0)][frontier_size]
#    vals = non_overlapping(table, (1.0, 1.0), frontier_size, exclude={0,1})
#    plt.plot(list(range(1,1+len(vals))), vals, label=f"{frontier_size}")
#plt.savefig('res.eps')
#plt.legend(fontsize=14)
#plt.ylabel('% tasks solved', size=16)
#plt.xlabel('iteration', size=16)
#plt.savefig(OUTPUT)


# Use this if you want to visualize the entire dataset
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
plt.savefig(OUTPUT)
