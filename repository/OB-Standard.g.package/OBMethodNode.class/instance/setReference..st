initializing
setReference: aMethodReference
	self 
		setSelector: aMethodReference methodSymbol
		class: (aMethodReference actualClass)