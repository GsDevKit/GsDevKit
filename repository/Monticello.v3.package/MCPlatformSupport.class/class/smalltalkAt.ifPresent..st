smalltalk
smalltalkAt: aGlobalName ifPresent: aBlock
  | glob |
  glob := GsSession currentSession symbolList objectNamed: aGlobalName.
  ^ glob ~~ nil
    ifTrue: [ aBlock value: glob ]
    ifFalse: [ glob ]