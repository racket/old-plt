class A: pass
 
class B: pass

 
class C(A, B):
    some_static_field = 7
    another_static_field = 3

    def m(this, x):
        return C.some_static_field + x
 
 
#Welcome to DrScheme, version 203.10-cvs27apr2003.
#Language: Python.
#> C
#<type 'C'>
#> C.m
#<unbound method C.m>
#> C.some_static_field
#7
#> myc = C()
#> myc
#<C object>
#> myc.m
#<bound method C.m of <C object>>
#> myc.some_static_field
#7
#> myc.some_static_field = 3
#> myc.some_static_field
#3
#> C.some_static_field
#7
#> myc.m(2)
#9