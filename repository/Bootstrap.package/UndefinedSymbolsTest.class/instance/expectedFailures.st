tests
expectedFailures

  ^ ((System stoneVersionAt: 'gsVersion') beginsWith: '2.')
    ifTrue: [ #( #testUndefinedSymbols ) ]
    ifFalse: [ #() ]