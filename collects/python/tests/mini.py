#def g():
#    try:
#        return 7
#    finally:
#        print "test"
#g()

#def h():
#    try:
#        return 7
#    finally:
#        return 8
#h()

#import module_b
#from module_b import f as moo
#import module_a
#print module_a.z
#module_a.z = 3
#print module_a.z


#import module_b as mb
#mb
#foo
#foo.g
#foo.g(1)

#from module_b import g as z
#z
#z(1)

#def add_one(x):
 #   return x + 1

#add_one(2)

#class C(A, B):
#    some_static_field = 7
#    another_static_field = 3

#    def m(this, x):
#        return C.some_static_field + x

print 1 is 1

x = 1
x = y = z = 2

a,b = (3,4)

def f():
  x = 1

class A(object): pass

class B(object): pass

class C(A, B):
  some_static_field = 7
  #print "hello!", some_static_field
  another_static_field = some_static_field + 3

  def m(this, x):
    return C.some_static_field + x


def f(pos_arg1, pos_arg2, def_arg=1, *rest, **kw_args):
  return 1

def f(x):
    return x

f.__call__

a = 3

def g(x):
    def h(y):
        return y
    return x + 2

class C:
    def m(this):
        return 8

    x = 2
    y = 3
    y = 4

c = C()
print c.m()
print c.x
print c.y

############ test the PyString C module ######
#P = pstring.PString
#p = P(4)
#p
#woot = P("woot")
#woot
#woot.upper()
##############################################

#x = 3
#while x > 0: print x; x-= 1

#x = [1,2,3]
#x[0] = 7
#print x

#a = 1
#b = 2
#a, b=b, a
#print a,b

#x = [0,1]
#i = 0
#i, x[i] = 1,2
#print x

#for x in [1,2,3]:
#    print "start of suite"
#    if  x == 3: break
#    elif x == 1: continue
#    else: print x
#    print "end of suite"
#else:
#    print "for loop done"



#try:
#    print "before raising"
#    #raise Exception, (1,2)
#    print "after raising??"
#except Exception, (x, y):
#    print "x:",x,"y:",y
#else:
#    print "else..."

#try:
#    print "before raising"
#    raise TypeError
#    print "after raising"
#except TypeError, err:
#    print "type error caught: ", err
#else:
#    print "else..."
#print "outside"

############### this fails!
#x = 2
#loop = 2
#while x > 0:
#    print x
#    print "loop: ",loop
#    x -= 1;
#else:
#    print "done!"

#tiffany
#2785120
#pirana
#2785108

#x = 3
#x += 2
#print x

#x = 7
#y = z = x
#(a, b, (c,d), z) = (x, y, (x,y), x)
#print "a:",a,"b:",b,"c:",c,"d:",d,"x:",x,"y:",y,"z:",z

#def f():
#    x = 7
#    y = z = x
#    (a, b, (c,(d)), z) = (x, y, (x,(y)), x)
#    print "a:",a,"b:",b,"c:",c,"d:",d,"x:",x,"y:",y,"z:",z

#f()
#map

#def f(x, y, (g, h, (i,j), k), m, n, (o, (p, q)), z = 3, a = 9, *rest, **more):
#    print "x:", x
#    ooo = 3
#    print "y:", y
#    print "z:", z
#    print "a:", a
#    print "rest:", rest
#    print "more:", more
#    print "--------------"

#f(x,y,(a,b,(c,d),e),z)

#f( y = 1, whee = 7, x = 2, a = 3, mooo = 9 )
#f( 0, 1, 2, 3, 4, 5, 6, 7 )

#def pr(name, val):
#    print name,val
#    return val

#g = lambda x, y: 3 #, z = 4: pr("x:",x) + pr("y:",y) + pr("z:",z)

#g(  0, z = 1, y = 2 )

#print f
#print g



#for x,y in [(1,2), (3,4)]: print "x:",x,"y:",y

#def f():
#    for x in [1,2,3]: print x
#    print x
#f()


#if 0:
#    print "zero"
#elif None:
#    print "none"
#elif 1:
#    print "one"
#else:
#    print "else"

#class e(object):
#    def m():
#        pass

#isinstance(e(), e)

#raise e
#raise e, e()

#raise Exception
#raise Exception, "test"

#raise "test"
#raise "test", "test again"



#assert 1
#print "assert passed as it should"
#assert 0
#assert 0, "test"

#x = [1,2,3]
#y = (4,5,6)
#z = [7,8,9]
#a = x[1:2]
#b = y[1:2]
#c = z.__getitem__(slice(1,2))
#print a
#print b
#print c


#map( lambda x,y: x + y, [1,2,3], [1,2,3] )
#map

#f = [(1,2), (3,4), (5,6)]
#g = [sqrt(x*x+y*y) for x,y in f]
#print g
#h = [sqrt(x*x+y*y) for [x,y] in f]
#print h


#arr = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
## The subject should be regular, with all rows the same length
#print [[r[col] for r in arr] for col in range(len(arr[0]))]


#xs = (1,2,3,4,5)
#ys = (9,8,7,6,5)
#bigmuls = [(x,y) for x in xs for y in ys if x*y > 25]
#print bigmuls


#a = [-3, 5, 2, -10, 7, 8]
#b = ["a", "b", "c"]
#c = [(x,y) for x in a for y in b if x > 0]
#c

#rhs = 3
#x = y = z = 4
#x
#rhs

#x = 7
#z = (x,y) = 1,2
#x
#y
#z
#lst = [a,b] = [3,4]
#a
#b
#lst

#[x,y] = 2

#def f():
#    def g():
#        print h()
#    def h():
#        return 1
#    print "first call"
#    g()
#    def i():
#        return 2
#    (x, h, z) = (1, i, 3)
#    print "second call"
#    g()



#y = {'a': 3, 'y': 2}
#y['a']

#x = 7
#x = 8

#def f():
#    print "this should appear once"
#    return 2

#1 < f() > 3

#not 1

#y = 1
#y = 2

#def f(z):
   # global y
   # x = 9
#    y = 3
#    x = 10
#    def g(a):
#        b = 3

# module scope: (x y)
# f local scope: (x z)
# f global scope: (y)
