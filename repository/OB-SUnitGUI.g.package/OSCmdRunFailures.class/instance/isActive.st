testing
isActive

	super isActive ifFalse: [ ^false ].
	self browser selectionPath detect: [:each | each isTestNode] ifNone: [^false ].
	^target root hasFailures