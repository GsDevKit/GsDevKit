shout
gsParserResolvePartial: aString forClass: classOrMetaClass
	false ifTrue: [ ^nil ].
	classOrMetaClass theNonMetaClass withAllSuperclasses do: [:c | | cp |
		(cp := c classPool) ~~ nil 
			ifTrue: [ (cp hasBindingThatBeginsWith: aString) 
							ifTrue: [^#incompleteIdentifier ]].
		c sharedPools do: [:p | (p hasBindingThatBeginsWith: aString) ifTrue: [^#incompleteIdentifier]].
		(c environment hasBindingThatBeginsWith: aString) ifTrue: [^#incompleteIdentifier]].
	^nil