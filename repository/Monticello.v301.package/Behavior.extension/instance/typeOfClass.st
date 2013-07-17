*monticello
typeOfClass
    "Answer a symbol uniquely describing the type of the receiver"
    self isBytes ifTrue:[^ #bytes].
    (self isIndexable _and: [superClass isIndexable not]) ifTrue:[^#variable].
    ^#normal.
