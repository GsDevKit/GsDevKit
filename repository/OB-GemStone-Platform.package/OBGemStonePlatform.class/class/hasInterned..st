smalltalk
hasInterned: aString
	^ self hasInterned: aString ifTrue: [:ignored | ]
