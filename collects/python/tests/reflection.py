class ClassA:                    # Define ClassA
      def setdata(self, value):
        self.data = value
      def display(self):
        print 'Class A data: ', self.data
class ClassB:                    # Define ClassB
      def output(self):
        print 'Class B data: ', self.data

newobject = ClassA()             # Create new instance of ClassA
newobject.setdata(23)            # Set data for this instance to 23
newobject.display()              # Use the ClassA method           
#output: Class A data: 23

newobject.__class__=ClassB       # Change it to a ClassB object!
newobject.output()               # Use the ClassB method
#output: Class B data: 23

#newobject.display()              # ClassA method doesn't exist
#output: Traceback (most recent call last):   # Returns error
#  File "", line 1, in ?
#    newobject.display()
#AttributeError: ClassB instance has no attribute 'display'

newobject.__class__=ClassA       # Change it back to ClassA
newobject.display()              # Use the ClassA method
#output: Class A data: 23
