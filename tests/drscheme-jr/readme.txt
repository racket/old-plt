The file all-tests.scm contains a unit that imports the signature:
    
    test-environment^
    
That signature contains various definitions of names of language levels and 
it contains the function `try':

  (try input output)
  where:  input : string
          output : (union string (list 'error regexp) 'void)

Try should type `input' into the repl, fetch the result and compare it with
`output'.  If they are the same, it should return #t, otherwise it should
return #f. Try is responsible for reporting any mismatches from the input
and output. If `output' is a string, it must be the same as the (non-error)
output. If `output' is a list whose first element is the symbol error, the
output must be an error and the regexp must match that error.  If `output'
is the symbol 'void, there must be no output.

Currently, auto.ss runs this tests suite in drscheme jr and the debug tool
runs this test suite in DrScheme.