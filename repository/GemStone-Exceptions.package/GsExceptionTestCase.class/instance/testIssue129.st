tests
testIssue129

   self deny: ((Error , Halt) handlesCategory: GemStoneError number: 6011). 
   self assert: ((Error , Halt) handlesCategory: GemStoneError number: 6001).
