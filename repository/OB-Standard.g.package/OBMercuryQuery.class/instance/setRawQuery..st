initialization
setRawQuery: aString 
	| tokens |
	tokens := Scanner new scanTokens: aString.
	pattern := tokens at: 1.
	isWild := (tokens at: 2 ifAbsent: []) = #*.
	^ self