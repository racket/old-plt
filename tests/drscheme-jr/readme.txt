The file all-tests.scm contains a unit that imports the signature:
    
    test-environment^
    
That signature contains various definitions of names of language levels and 
it contains the function `try':

  (try input output)
  where:  input : string
          output : (union string (list 'error regexp))

Try should type `input' into the repl, fetch the result and compare it with
`output'.  If they are the same, it should return #t, otherwise it should
return #f. Try is responsible for reporting any mismatches from the input
and output. If `output' is a string, it must be just the regular otuput and
if it is a list starting with the symbol 'error, it must be an error
message that matches the regexp.

Currently, auto.ss runs this tests suite in drscheme jr and the debug tool
runs this test suite in DrScheme.