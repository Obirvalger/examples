import matplotlib.pyplot as plt
from math import log

ys  = []
xss = []

for line in open("in.csv"):
    *xs, y = map(int, line.split())
    xss.append(xs)
    ys.append(y)

xss = [list(x) for x in zip(*xss)] # transpose

for i in range(len(ys)):
    if (ys[i] == 0):
        marker = 'o'
        c = 'r'
    else:
        marker = 'x'
        c = 'b'
    plt.scatter(xss[0][i], xss[1][i], marker=marker, c=c)

plt.show()

def entropy(ys):
    n  = len(ys)
    n0 = len([x for x in ys if x == 0])
    n1 = len([x for x in ys if x == 1])
    return - (n0 / n) * log(n0 / n, 2) - (n1 / n) * log(n1 / n, 2)

class MyData:
    def __init__(self, xss, ys):
        self.xss = xss
        self.ys = ys

    def entropy(self):
        n  = len(self.ys)
        n0 = len([x for x in self.ys if x == 0])
        n1 = len([x for x in self.ys if x == 1])
        return - (n0 / n) * log(n0 / n, 2) - (n1 / n) * log(n1 / n, 2)
    
    def uniq(self, i):
        return list(set(self.xss[i]))
    
    def IG(self, i):
        l = self.uniq(i)
        
        for n in l:
            yl = [self.ys[j] for j, x in enumerate(self.xss[i]) if x == n]
            print(yl)
            print(entropy(yl))
            
        return yl
        

m = MyData(xss,ys)
