*core-squeak
instanceVariableNames: instanceVariableNames

  ^self thisClass isInvariant
    ifTrue: [
      self thisClass _beVariantWhile: [
        self addInstVarNames: (instanceVariableNames findTokens: ' ') asArray]]
    ifFalse: [ self addInstVarNames: (instanceVariableNames findTokens: ' ') asArray ].
