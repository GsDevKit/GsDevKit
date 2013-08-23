converting
referenceToMethod: aSelector
	| ref |
	ref := MethodReference new.
	ref setClassSymbol: name classIsMeta: isMeta  methodSymbol: aSelector stringVersion: ''.
	^ ref