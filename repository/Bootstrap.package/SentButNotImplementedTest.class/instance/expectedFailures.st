tests
expectedFailures

  ^ ((System stoneVersionAt: 'gsVersion') beginsWith: '2.')
    ifTrue: [ #( #testSentButNotImplemented ) ]
    ifFalse: [ #() ]