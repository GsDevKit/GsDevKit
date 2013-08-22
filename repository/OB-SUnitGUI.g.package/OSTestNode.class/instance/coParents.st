multiple selection
coParents

	coParents == nil
		ifTrue: [ ^Array with: self ].
	^coParents