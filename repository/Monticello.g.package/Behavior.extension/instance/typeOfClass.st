*monticello
typeOfClass
	"Answer a symbol uniquely describing the type of the receiver"
	self isBytes ifTrue:[^#bytes].
	self _portableIsIndexable ifTrue:[^#variable].
	(format bitAnd: 16r1000) == 16r1000 ifTrue: [ ^#transient ].
	^#normal.