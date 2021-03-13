configuration
defaultMetaNodeFor: aNode
	| element |
	element := OTChasingMetaNode new
		addFilter: OTChasingInspectorFilter new;
		yourself.
	aNode isLastNode ifTrue: [ ^element ].
	aNode filterPragmas do: [:pragma |
		element childAt: pragma selector labeled: (pragma argumentAt: 1) asString put: element ].
	^element