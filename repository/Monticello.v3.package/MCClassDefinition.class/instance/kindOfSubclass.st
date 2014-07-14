printing
kindOfSubclass
  "for printing only"
   | ty |
   (ty := self type) _isArray ifTrue:[ ty := ty at: 1 ].
    ty == #normal ifTrue: [^ ' subclass: '].
    ty == #variable ifTrue: [^ ' variableSubclass: '].
    ty == #indexable ifTrue: [^ ' variableSubclass: '].
    ty == #bytes ifTrue: [^ ' variableByteSubclass: '].
    self error: 'Unrecognized class type'
