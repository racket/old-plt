x = 1
x = y = 2

def f():
    print "function start"
    print "x is at first:", x
    x = 1
    print "x is now: ", x

x

z = 3
def g():
    global z
    z = 4

z