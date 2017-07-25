*bootstrap
executeShould: aBlock inScopeOf: anObject 
  | expectedErrorNumber |
  (anObject isKindOf: Integer) ifTrue: [
    expectedErrorNumber := anObject.
  ] ifFalse: [(anObject isKindOf: Symbol) ifTrue: [
    expectedErrorNumber := ErrorSymbols at: anObject.
  ] ifFalse: [
    ^[aBlock value.
       false] sunitOn: anObject
      do: [:ex | ^true]
  ]].
  ^[aBlock value.
   false] sunitOn: Error
    do: [:ex | 
      expectedErrorNumber = ex number ifTrue: [^true]]