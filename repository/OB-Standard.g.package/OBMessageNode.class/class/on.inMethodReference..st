instance creation
on: aSelector inMethodReference: ref
     (ref respondsTo: #enhancedOn:inMethodReference:) 
		ifTrue: [^self perform: #enhancedOn:inMethodReference: with: aSelector with: ref].
	^ self 
		on: aSelector 
		inMethod: ref methodSymbol
		inClass: ref actualClass