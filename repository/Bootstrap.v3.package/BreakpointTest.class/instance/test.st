tests
test
 
  | caughtIt resumed meth |
  meth := self class compiledMethodAt: #breakMethod.
  meth setBreakAtStepPoint: 1.  

  "catch and resume"
  caughtIt := resumed := false.
  [ self breakMethod  .
    resumed := true 
  ] on: Breakpoint do:[:ex |
    caughtIt := true.
    ex resume
  ].
  self assert: caughtIt; assert: resumed .

  "catch and ignore"
  caughtIt := resumed := false.
  [ self breakMethod  .
    resumed := true 
  ] on: Breakpoint do:[:ex |
    caughtIt := true.
  ].
  self assert: caughtIt; deny: resumed .

  "catch and return"
  caughtIt := resumed := false.
  [ self breakMethod  .
    resumed := true 
  ] on: Breakpoint do:[:ex |
    caughtIt := true.
    ex return
  ].
  self assert: caughtIt; deny: resumed .