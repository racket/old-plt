#!/arch/unix/bin/python

# ------------------------------------------------------------------------------
# (Listof (list String String String)) -> (Listof String)
def winner_takes_all(loe):
    "compute the winners of the subelection according to winner takes all"
    return determine_winners(count_wta(loe))

# (Listof (list String String String)) -> (Dictionaryof String Number)
def count_wta(loe):
    "count the votes according to winner takes all"
    counter = {}
    for x in loe:
	bump(counter,x[0],1)
    return counter

# ------------------------------------------------------------------------------
# (Listof (list String String String)) -> (Listof String)
def points_per_place(loe):
    "compute the winners of the subelection according to points per place"
    return determine_winners(count_ppp(loe))

# (Listof (list String String String)) -> (Dictionaryof String Number)
def count_ppp(loe):
    "count the votes according to points per place"
    counter = {}
    for x in loe:
	bump(counter,x[0],3)
	bump(counter,x[1],2)
	bump(counter,x[2],1)
    return counter

# ------------------------------------------------------------------------------
# (Listof (list String String String)) -> (Listof String)
def approval_rating(loe):
    "compute the winners of the subelection according to approval rating"
    return determine_winners(count_apr(loe))

# (Listof (list String String String)) -> (Dictionaryof String Number)
def count_apr(loe):
    "count the votes according to a approval rating"
    counter = {}
    for x in loe:
	bump(counter,x[0],1)
	bump(counter,x[1],1)
    return counter

# ------------------------------------------------------------------------------
# Auxiliaries:

# (Dictionaryof String Number) String Number -> Void

def bump(d,y,n):
    "increase the counter in a dictionary"
    d[y] = d.setdefault(y,0) + n

# (Dictionaryof String Number) -> (Listof String)
def determine_winners(l):
    "determine the winning entry in the dictionary"
    m = max(l.values())
    r = []
    #print "l:", l
    #print "m:", m
    for i in l.keys():
        #print "i:", i
	#print "l[i]:", l[i]
	if l[i] == m:
	   #print "l[i] == ", m
	   r = r + [i]
	#print "l[i] not equal"
    #print "finished iterating"
    r.sort()
    return r

print winner_takes_all([['chicken','tofu','steak'],['steak','chicken','tofu']])
print points_per_place([['chicken','tofu','steak'],['steak','chicken','tofu']])
print approval_rating([['chicken','tofu','steak'],['steak','chicken','tofu']])

def f( (x,y) ):
   pass

  