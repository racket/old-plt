import define_x
print "at first, x is two: ", define_x.x
define_x.x = 1
print "you should see the number one: ", define_x.x
import redefine_x
print "now x should be seven: ", define_x.x

