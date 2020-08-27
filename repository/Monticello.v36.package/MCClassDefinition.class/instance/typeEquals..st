comparing
typeEquals: otherType

| ty |
(ty := self type) _isSymbol ifTrue:[ ^ otherType == ty ].
(ty _isArray and:[ otherType _isArray]) ifTrue:[
  (ty at: 1) == (otherType at: 1)  ifFalse:[ ^ false ].
  ^ (ty at:2) asIdentitySet = (otherType at:2) asIdentitySet
].
^ false
