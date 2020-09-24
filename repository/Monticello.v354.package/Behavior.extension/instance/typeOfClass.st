*monticello
typeOfClass
  "Answer a symbol uniquely describing the type of the receiver"

  self isBytes
    ifTrue: [ ^ #'bytes' ].
  self _portableIsIndexable
    ifTrue: [ ^ #'variable' ].
  ^ #'normal'