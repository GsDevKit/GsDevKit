public
text: aString
	self isReadOnly ifTrue: [ ^ false ].
	self value: (aString
		evaluateInContext: self object 
		symbolList: GsSession currentSession symbolList).
	^ true