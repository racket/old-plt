2
#x = 2
#print x
#x = x + 2

#print x

#def f(x):
#    return x + 1;
#print f(1)

#x.__repr__()

#print x.__add__( x.__sub__(1) )

#r = x.__repr__


#str_x = r()

#str_x

#print str_x

#hello = 'hello'

#hello.__repr__()

#print hello.__repr__()

#def call_with_nine(fun):
#    return fun(9)

#print call_with_nine(f)

#call_with_nine

#print call_with_nine

#print {'a':1, 'b':9}

#print f

#define = 3

print "type(1):", type(1)


class c(object):
    print 7
    value = 0
    def __init__(this, v):
        def g(x):
            return x + 1
        this.value = g(v)
    def m(this, x):
        return this.value + x
    
    def n(this, y):
        return y - 1
    
    print 22
    
    x = 3
    y = staticmethod(x)
    print "before s"
    def s(a, b):
        return a + b
    print "after s"
    sm = staticmethod(s)
    print "after sm"

    def cm(self_cls, v):
        return self_cls(v)
    cm = classmethod(cm)

class d(c):
    print 2


myc = c(1)
print "myc value:",myc.value
print "myc.m:", myc.m
print "c.m:", c.m
print myc.m(9)

cn = c.n

print cn(myc, 1)

myc.val = 9
print "myc.val:", myc.val
c.sm
print "c.sm(1,2):", c.sm(1,2)

sm = c.sm
print "sm(3,4):", sm(3,4)

print "c.cm(2):", c.cm(2)

cm = c.cm
cm
print "cm(3):", cm(3)

object.__init__


class k(object):
    def __getattribute__(this, key):
        print "__getattribute__ called by", this, "looking for", key
    
    def __setattr__(this, key, value):
        print "__setattr__ called by", this, "trying to set", key, "to", value
    
    def __getattr__(this, key):
        print "__getattr__ called instead of getattribute"
        


