tests
testUndefinedSymbols
  | undefined |
  self cleanUndefinedSymbols.
  undefined := UndefinedSymbols keys.
  undefined removeAllPresent: self specialCases.
  undefined
    do: [ :sel | 
      Transcript
        cr;
        show: 'Undefined Symbol:' , sel printString ].
  self assert: undefined isEmpty