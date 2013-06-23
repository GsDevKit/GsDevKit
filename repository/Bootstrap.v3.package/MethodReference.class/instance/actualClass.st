queries
actualClass 

	| actualClass |
	actualClass := self symbolDictionary ~~ nil
		ifTrue: [ 
			self symbolDictionary 
				at: classSymbol 
				ifAbsent: [ System myUserProfile symbolList objectNamed: classSymbol ] ]
		ifFalse: [ System myUserProfile symbolList objectNamed: classSymbol ].
	actualClass == nil ifTrue: [^nil].
	classIsMeta ifTrue: [^actualClass class].
	^actualClass