tests
testSentButNotImplemented
  | sentNotImplemented |
  sentNotImplemented := self sentButNotImplemented.
  sentNotImplemented
    keysAndValuesDo: [ :meth :selectors | 
      Transcript
        cr;
        show: 'Sent but not implemented from ' , meth printString.
      selectors
        do: [ :sel | 
          Transcript
            cr;
            tab;
            show: sel printString ] ].
  self assert: sentNotImplemented isEmpty