*core-squeak-subclass creation
copyVariables
| chSize chist priorVersion priorVars civNames priorCivNames toIgnoreCount |
(chSize := (chist := classHistory) size) = 1 ifTrue:[ 
  ^ self
].
priorVersion := chist at: chSize - 1.
(priorVars := priorVersion _classVars) notNil ifTrue: [
  | cvars |
  cvars := classVars.
  priorVars associationsDo: [:anAssociation | |aKey |
    anAssociation value == priorVars
      ifTrue: [
	  "ensure creation of class var symbol dictionary"
	  self _createClassVarsDict name: anAssociation key ]
      ifFalse: [
        cvars notNil ifTrue: [ 
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
