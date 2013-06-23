tests
testSentButNotImplemented
  | sentNotImplemented |
  (sentNotImplemented := self sentButNotImplemented)
    removeAllPresent: self specialCases.
  sentNotImplemented
    do: [ :sel | 
      Transcript
        cr;
        show: 'Sent but not implemented: ' , sel printString ].
  self assert: sentNotImplemented isEmpty