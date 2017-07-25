smalltalk
smalltalkAt: aGlobalName ifAbsent: aBlock
  | glob |
  glob := GsSession currentSession symbolList objectNamed: aGlobalName.
  ^ glob == nil
    ifTrue: [ aBlock value ]
    ifFalse: [ glob ]