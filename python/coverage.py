#! /usr/bin/python3

import numpy as np

n = 10

def is_coverage(a):
    return a.sum(0).min()

a = np.random.randint(0, 2, (n,n))
while not is_coverage(a):
    a = np.random.randint(0, 2, (n,n))


def greedy(a, coverage):
    while a.size > 0:
        n = a.sum(1).argmax()
        #  print("N:\n", n)
        add_to_coverage(coverage, n)
        b = a[n]
        #  print("B:\n", b)
        a = a[...,[i for i in range(b.size) if not b[i]]]
        a = np.delete(a, n, axis=0)
        #  print("A:\n",a)


def add_to_coverage(cov, n):
    # use inf as bound to clarify algorithm
    cov.append(float('inf'))
    for i, v in enumerate(cov):
        #  print('i={};v={} '.format(i,v), end='')
        if n+i < v:
            cov.insert(i, n+i)
            break
    # delete inf
    cov.pop()
    return cov

if __name__ == "__main__":
    cov = []
    print("Initial:\n{}".format(a))
    greedy(a, cov)
    #  print(cov)
    coverage = a[cov]
    print("Coverage:\n{}".format(coverage))
    print("Size: ", coverage.shape[0], "\tIndexes: ", cov)
# 0
# 1
# 2
# 3
# 4
# 5
# 6
    #  for i in [4,3,1,1,2]:
        #  print(add_to_coverage(cov, i))
