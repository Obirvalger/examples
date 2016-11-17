from collections import Counter
from math import log

def readData(fname):
    rows = []
    for line in open(fname):
       rows.append(list(map(int, line.split())))
    
    return(rows)

class decisionnode:
    def __init__(self,col=-1,value=None,results=None,tb=None,fb=None):
        self.col=col # column index of criteria being tested
        self.value=value # vlaue necessary to get a true result
        self.results=results # dict of results for a branch, None for everything except endpoints
        self.tb=tb # true decision nodes 
        self.fb=fb # false decision nodes

def divideset(rows,column,value):
    # Make a function that tells us if a row is in the first group 
    # (true) or the second group (false)
    split_function=None
    # for numerical values
    if isinstance(value,int) or isinstance(value,float):
        split_function = lambda row: row[column] <= value
    # for nominal values
    else:
        split_function = lambda row: row[column] == value
   
    # Divide the rows into two sets and return them
    set1=[row for row in rows if split_function(row)] # if split_function(row) 
    set2=[row for row in rows if not split_function(row)]
    return (set1,set2)

def uniquecounts(rows):
    return dict(Counter(x[-1] for x in rows))

def entropy(rows):
    log2 = lambda x: log(x,2)  
    results = uniquecounts(rows)
    ent, n = 0, len(rows)
    for r in results:
        # current probability of class
        p = results[r] / n 
        ent -= p*log2(p)
    return ent

def fit(rows, scorefun=entropy):
    if len(rows) == 0: return decisionnode()
    current_score = scorefun(rows)

    best_gain = 0.0
    best_criteria = None
    best_sets = None

    column_count = len(rows[0]) - 1	# last column is result
    for col in range(0, column_count):
        # find different values in this column
        column_values = set(row[col] for row in rows)

        # for each possible value, try to divide on that value
        for value in column_values:
            set1, set2 = divideset(rows, col, value)

            # Information gain
            p = len(set1) / len(rows)
            gain = current_score - p*scorefun(set1) - (1-p)*scorefun(set2)
            if gain > best_gain and len(set1) > 0 and len(set2) > 0:
                best_gain = gain
                best_criteria = (col, value)
                best_sets = (set1, set2)

    if best_gain > 0:
        trueBranch = fit(best_sets[0])
        falseBranch = fit(best_sets[1])
        return decisionnode(col=best_criteria[0], value=best_criteria[1],
                tb=trueBranch, fb=falseBranch)
    else:
        #return decisionnode(results=uniquecounts(rows))
        return decisionnode(results=rows[0][-1])

def predictRow(tree, row):
    if tree.results != None:
        return tree.results
    if row[tree.col] <= tree.value:
        return predictRow(tree.tb, row)
    else:
        return predictRow(tree.fb, row)
        
def predict(tree, rows):
    res = []
    for row in rows:
        res.append(row + [predictRow(tree, row)])
    
    return res

def test(rows, n):
    t = fit(rows[:n])
    ys = predict(t, [x[:-1] for x in rows[n:]])
    right = sum(1 for i in range(n, len(rows)) if rows[i] == ys[i-n])
    return (right, len(rows) - n - right)

def printtree(tree,indent=''):
    # Is this a leaf node?
    if tree.results != None:
        print(str(tree.results))
    else:
        # Print the criteria
        print('Column ' + str(tree.col)+' : '+str(tree.value)+'? ')

        # Print the branches
        print(indent+'True->', end='')
        printtree(tree.tb,indent+'  ')
        print(indent+'False->', end='')
        printtree(tree.fb,indent+'  ')
