
def f(x, y, z, *rest, **dict):
    print "x:", x, "y:", y, "z:", z
    print "rest:", rest
    print "dict:", dict
#    return foldr

f(1,2,3,4,5,6,woot=8)
#f(1,z=2,y=3,woot=4)
