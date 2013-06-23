*core-squeak-subclass creation
copyVariables
		"bug #40559 and #bug40822 - copy class (instance) variables to new version"

| chSize chist priorVersion priorVars civNames priorCivNames toIgnoreCount |
(chSize := (chist := classHistory) size) = 1 ifTrue:[ 
  ^ self
].
priorVersion := chist at: chSize - 1.
(priorVars := priorVersion _classVars) notNil ifTrue: [
  | cvars |
  (cvars := classVars) notNil ifTrue: [
    priorVars associationsDo: [:anAssociation | |aKey |
      anAssociation value == priorVars
      	ifTrue: [ cvars addAssociation: (SymbolAssociation newWithKey: anAssociation key value: cvars) ]
      	ifFalse: [
        (cvars includesKey: (aKey := anAssociation key)) ifTrue: [
          cvars removeKey: aKey .
          cvars addAssociation: anAssociation.
        ].
      ].
    ].
  ].
].

priorCivNames := priorVersion class allInstVarNames.
toIgnoreCount := Class allInstVarNames size.
toIgnoreCount < priorCivNames size ifTrue: [
	priorCivNames := priorCivNames 
		copyFrom: toIgnoreCount + 1
		to: priorCivNames size.
	civNames := self class allInstVarNames.
	priorCivNames do: [:each | 
		(civNames includesIdentical: each) ifTrue: [
			self atClassInstVar: each put: (priorVersion atClassInstVar: each).
		].
	].
].
