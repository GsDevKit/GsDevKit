comparing
hash
    | hash ty |
    hash := String stringHash: name initialHash: 0.
    hash := String stringHash: superclassName initialHash: hash.
    hash := String stringHash: self traitCompositionString initialHash: hash.
    hash := String stringHash: self classTraitComposition asString initialHash: hash.
    hash := String stringHash: (category ifNil: ['']) initialHash: hash.
    (ty := self type) _isArray ifTrue:[ ty := ty at: 1 ].
    hash := String stringHash: ty initialHash: hash.
    variables do: [ :v |
        hash := String stringHash: v name initialHash: hash.
    ].
    ^ hash