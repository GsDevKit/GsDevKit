tests
testResumeAndSignal
 
  | caughtIt caughtWarning resumed meth |
  meth := self class compiledMethodAt: #breakMethod.
  meth setBreakAtStepPoint: 1.  

  caughtWarning := caughtIt := resumed := false.
  [ self breakMethod  .
    resumed := true.
    Warning signal: 'hi there'.
  ] on: Warning, Breakpoint do:[:ex |
    caughtIt := true.
    (ex isKindOf: Warning) ifTrue: [  caughtWarning := true ]. 
    ex resume
  ].
  self assert: caughtIt; assert: resumed; assert: caughtWarning.

  caughtWarning := caughtIt := resumed := false.
  [ self warningBreakMethod  .
    resumed := true.
  ] on: Warning, Breakpoint do:[:ex |
    caughtIt := true.
    (ex isKindOf: Warning) ifTrue: [  caughtWarning := true ]. 
    ex resume
  ].
  self assert: caughtIt; assert: resumed; assert: caughtWarning.

  meth := self class compiledMethodAt: #warningBreakMethod.
  meth setBreakAtStepPoint: 1.  

  caughtWarning := caughtIt := resumed := false.
  [ self warningBreakMethod  .
    resumed := true.
  ] on: Warning, Breakpoint do:[:ex |
    caughtIt := true.
    (ex isKindOf: Warning) ifTrue: [  caughtWarning := true ]. 
    ex resume
  ].
  self assert: caughtIt; assert: resumed; assert: caughtWarning.