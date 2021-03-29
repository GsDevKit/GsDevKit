tests
testIssue127
  "https://github.com/GsDevKit/GsDevKit/issues/127"

  "multiple undefined symbols in a method"

  | undefined cat |
  self cleanUndefinedSymbols.
  cat := #'issue 127'.
  (self class categories includes: cat)
    ifTrue: [ self class removeCategory: cat ].
  self cleanUndefinedSymbols.
  [ self class compileMethod: self _issue127Source category: cat ]
    ensure: [ 
      (self class categories includes: cat)
        ifTrue: [ self class removeCategory: cat ].
      self cleanUndefinedSymbols ]
