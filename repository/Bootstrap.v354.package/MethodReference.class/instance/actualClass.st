queries
actualClass
  | actualClass |
  actualClass := self symbolDictionary ~~ nil
    ifTrue: [ 
      self symbolDictionary
        at: classSymbol
        ifAbsent: [ GsSession currentSession symbolList objectNamed: classSymbol ] ]
    ifFalse: [ GsSession currentSession symbolList objectNamed: classSymbol ].
  actualClass == nil
    ifTrue: [ ^ nil ].
  classIsMeta
    ifTrue: [ ^ actualClass class ].
  ^ actualClass