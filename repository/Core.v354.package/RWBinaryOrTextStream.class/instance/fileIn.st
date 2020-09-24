fileIn/Out
fileIn

  | val |
  [self atEnd] whileFalse:  [
    self skipSeparators.
    val := (self peekFor: $!) 
      ifTrue: [ | chunk classReader |
        chunk := self nextChunk.
        classReader := chunk evaluate.
        classReader scanFrom: self]
      ifFalse: 
        [self nextChunk evaluate]  
  ].
  ^val